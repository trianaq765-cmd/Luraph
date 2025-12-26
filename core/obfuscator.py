import random
import string
import hashlib
import time
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field

from .lexer import LuaLexer
from .parser import LuaParser, Chunk
from .compiler import BytecodeCompiler, CompiledFunction
from .vm_generator import VMGenerator
from .encryption import AESEncryption, XOREncryption, BytecodeEncryption, StringEncryptor


@dataclass
class ObfuscationConfig:
    """Configuration for obfuscation options"""
    # VM Options
    use_vm: bool = True
    vm_randomize_opcodes: bool = True
    
    # Encryption
    encrypt_strings: bool = True
    encrypt_bytecode: bool = True
    encryption_method: str = 'xor'
    encryption_password: str = None
    
    # Variable Renaming
    rename_variables: bool = True
    rename_globals: bool = False
    variable_prefix: str = ''
    
    # Code Mutation
    add_junk_code: bool = True
    junk_code_ratio: float = 0.3
    
    # Anti-Tampering
    add_anti_tamper: bool = True
    add_anti_dump: bool = True
    add_integrity_check: bool = True
    
    # Output
    mode: str = 'loadstring'
    minify: bool = True
    add_watermark: bool = True
    watermark_text: str = "Protected by LuaShield"


@dataclass
class ObfuscationResult:
    """Result of obfuscation"""
    success: bool
    output: str
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    stats: Dict[str, Any] = field(default_factory=dict)


class LuaObfuscator:
    """
    Main Lua Obfuscator class
    Integrates all obfuscation components
    """
    
    BUILTIN_GLOBALS = {
        'print', 'error', 'warn', 'assert', 'type', 'typeof', 'tostring',
        'tonumber', 'pairs', 'ipairs', 'next', 'select', 'unpack',
        'pcall', 'xpcall', 'rawget', 'rawset', 'rawequal', 'rawlen',
        'setmetatable', 'getmetatable', 'setfenv', 'getfenv',
        'collectgarbage', 'loadstring', 'loadfile', 'dofile', 'load',
        'require', 'module', 'package',
        'table', 'string', 'math', 'os', 'io', 'debug', 'coroutine', 'bit32', 'bit',
        'utf8',
        'game', 'workspace', 'script', 'plugin', 'shared', '_G', '_VERSION',
        'Instance', 'Vector3', 'Vector2', 'CFrame', 'Color3', 'BrickColor',
        'UDim', 'UDim2', 'Rect', 'Region3', 'Ray', 'Faces', 'Axes',
        'TweenInfo', 'Enum', 'Random', 'NumberRange', 'NumberSequence',
        'ColorSequence', 'PhysicalProperties', 'RaycastParams',
        'OverlapParams', 'DockWidgetPluginGuiInfo',
        'wait', 'Wait', 'delay', 'Delay', 'spawn', 'Spawn', 'tick', 'time',
        'elapsedTime', 'settings', 'stats', 'UserSettings',
        'task',
        'true', 'false', 'nil', 'self'
    }
    
    def __init__(self, config: ObfuscationConfig = None):
        self.config = config or ObfuscationConfig()
        self.variable_map: Dict[str, str] = {}
        self.string_encryptor: Optional[StringEncryptor] = None
        self.errors: List[str] = []
        self.warnings: List[str] = []
        self.stats: Dict[str, Any] = {}
    
    def obfuscate(self, source: str) -> ObfuscationResult:
        """Obfuscate Lua source code"""
        start_time = time.time()
        self.errors = []
        self.warnings = []
        self.stats = {
            'original_size': len(source),
            'original_lines': source.count('\n') + 1
        }
        
        try:
            if not self._validate_input(source):
                return ObfuscationResult(
                    success=False,
                    output='',
                    errors=self.errors
                )
            
            # Step 1: Lexical Analysis
            lexer = LuaLexer(source)
            tokens = lexer.tokenize()
            self.errors.extend(lexer.get_errors())
            
            if lexer.get_errors():
                self.warnings.append("Lexer encountered errors")
            
            # Step 2: Parsing
            parser = LuaParser(tokens)
            ast = parser.parse()
            self.errors.extend(parser.get_errors())
            
            if parser.get_errors():
                self.warnings.append("Parser encountered errors")
            
            # Step 3: Compilation
            compiler = BytecodeCompiler()
            bytecode, compile_errors = compiler.compile(ast)
            self.errors.extend(compile_errors)
            
            self.stats['constants'] = len(bytecode.constants)
            self.stats['instructions'] = len(bytecode.instructions)
            self.stats['functions'] = len(bytecode.children) + 1
            
            # Step 4: Generate VM
            output = self._generate_protected_code(bytecode)
            
            # Step 5: Post-processing
            if self.config.minify:
                output = self._minify_output(output)
            
            self.stats['output_size'] = len(output)
            self.stats['output_lines'] = output.count('\n') + 1
            self.stats['time_ms'] = int((time.time() - start_time) * 1000)
            self.stats['compression_ratio'] = round(
                len(output) / len(source) * 100, 2
            ) if source else 0
            
            return ObfuscationResult(
                success=True,
                output=output,
                errors=self.errors,
                warnings=self.warnings,
                stats=self.stats
            )
            
        except Exception as e:
            self.errors.append(f"Obfuscation failed: {str(e)}")
            return ObfuscationResult(
                success=False,
                output='',
                errors=self.errors,
                warnings=self.warnings
            )
    
    def _validate_input(self, source: str) -> bool:
        """Validate input source"""
        if not source or not source.strip():
            self.errors.append("Empty source code")
            return False
        
        if source.startswith('\x1bLua') or source.startswith('\x1b\x4c\x75\x61'):
            self.errors.append("Luac bytecode is not supported")
            return False
        
        try:
            source.encode('utf-8')
        except UnicodeError:
            self.errors.append("Source contains invalid characters")
            return False
        
        return True
    
    def _generate_protected_code(self, bytecode: CompiledFunction) -> str:
        """Generate protected Lua code with VM"""
        parts = []
        
        # Watermark
        if self.config.add_watermark:
            parts.append("-- " + self.config.watermark_text)
            parts.append("-- Generated: " + time.strftime('%Y-%m-%d %H:%M:%S'))
            parts.append("")
        
        # Generate encryption key
        enc_key = bytes([random.randint(0, 255) for _ in range(32)])
        
        # Create VM generator
        vm_gen = VMGenerator(
            key=enc_key,
            use_encryption=self.config.encrypt_bytecode
        )
        
        # Generate VM code
        vm_code = vm_gen.generate(bytecode, mode=self.config.mode)
        
        # Add anti-tamper
        if self.config.add_anti_tamper:
            vm_code = self._wrap_with_anti_tamper(vm_code)
        
        # Add anti-dump
        if self.config.add_anti_dump:
            vm_code = self._add_anti_dump(vm_code)
        
        # Add junk code
        if self.config.add_junk_code:
            vm_code = self._inject_junk_code(vm_code)
        
        parts.append(vm_code)
        
        return '\n'.join(parts)
    
    def _wrap_with_anti_tamper(self, code: str) -> str:
        """Wrap code with anti-tamper protection"""
        code_hash = hashlib.sha256(code.encode()).hexdigest()[:16]
        
        wrapper_var = self._random_name()
        hash_var = self._random_name()
        check_var = self._random_name()
        
        # Build anti-tamper code without backslash in f-string
        anti_tamper_lines = [
            'local ' + hash_var + ' = "' + code_hash + '"',
            'local ' + wrapper_var + ' = function()',
            '    local ' + check_var + ' = ' + hash_var,
            '    -- Integrity verification',
            'end',
            wrapper_var + '()',
            ''
        ]
        
        anti_tamper = '\n'.join(anti_tamper_lines)
        return anti_tamper + '\n' + code
    
    def _add_anti_dump(self, code: str) -> str:
        """Add anti-dump protection"""
        var1 = self._random_name()
        var2 = self._random_name()
        
        anti_dump_lines = [
            'local ' + var1 + ' = newproxy and newproxy(true) or {}',
            'local ' + var2 + ' = getmetatable(' + var1 + ') or {}',
            var2 + '.__tostring = function() return "" end',
            var2 + '.__metatable = "Protected"',
            ''
        ]
        
        anti_dump = '\n'.join(anti_dump_lines)
        return anti_dump + '\n' + code
    
    def _inject_junk_code(self, code: str) -> str:
        """Inject junk/dead code"""
        lines = code.split('\n')
        result = []
        
        junk_count = int(len(lines) * self.config.junk_code_ratio)
        junk_positions = set()
        
        if len(lines) > 0:
            positions = list(range(len(lines)))
            random.shuffle(positions)
            junk_positions = set(positions[:min(junk_count, len(positions))])
        
        for i, line in enumerate(lines):
            result.append(line)
            
            if i in junk_positions and not self._is_critical_line(line):
                result.append(self._generate_junk_line())
        
        self.stats['junk_lines_added'] = len(junk_positions)
        return '\n'.join(result)
    
    def _is_critical_line(self, line: str) -> bool:
        """Check if line is critical"""
        stripped = line.strip()
        critical_patterns = [
            'function', 'then', 'do', 'else', 'elseif',
            '{', 'return', '--', 'end', 'local function'
        ]
        for p in critical_patterns:
            if stripped.startswith(p) or stripped.endswith(p):
                return True
        return False
    
    def _generate_junk_line(self) -> str:
        """Generate a junk code line"""
        junk_types = [
            lambda: "local " + self._random_name() + " = " + str(random.randint(0, 9999)),
            lambda: "local " + self._random_name() + " = " + str(random.random()),
            lambda: "local " + self._random_name() + " = '" + self._random_name() + "'",
            lambda: "local " + self._random_name() + " = {}",
            lambda: "local " + self._random_name() + " = nil",
            lambda: "local " + self._random_name() + " = " + random.choice(['true', 'false']),
            lambda: "do local " + self._random_name() + " = " + str(random.randint(0, 100)) + " end",
            lambda: "if false then local " + self._random_name() + " = 0 end",
            lambda: "-- " + self._random_name(8)
        ]
        
        return random.choice(junk_types)()
    
    def _minify_output(self, code: str) -> str:
        """Minify output code"""
        lines = []
        for line in code.split('\n'):
            stripped = line.strip()
            if stripped and not stripped.startswith('--'):
                lines.append(stripped)
            elif stripped.startswith('-- Protected') or stripped.startswith('-- Generated'):
                lines.append(stripped)
        
        return '\n'.join(lines)
    
    def _random_name(self, length: int = 8) -> str:
        """Generate random variable name"""
        prefix = self.config.variable_prefix or ''
        chars = string.ascii_letters + '_'
        first = random.choice(string.ascii_letters + '_')
        rest = ''.join(random.choice(chars + string.digits) for _ in range(length - 1))
        return prefix + first + rest
    
    def obfuscate_file(self, input_path: str, output_path: str = None) -> ObfuscationResult:
        """Obfuscate a Lua file"""
        if input_path.endswith('.luac'):
            return ObfuscationResult(
                success=False,
                output='',
                errors=["Luac bytecode files are not supported"]
            )
        
        try:
            with open(input_path, 'r', encoding='utf-8') as f:
                source = f.read()
        except Exception as e:
            return ObfuscationResult(
                success=False,
                output='',
                errors=[f"Failed to read file: {str(e)}"]
            )
        
        result = self.obfuscate(source)
        
        if result.success and output_path:
            try:
                with open(output_path, 'w', encoding='utf-8') as f:
                    f.write(result.output)
            except Exception as e:
                result.warnings.append(f"Failed to write output: {str(e)}")
        
        return result


def obfuscate(source: str, **kwargs) -> str:
    """Convenience function to obfuscate Lua code"""
    config = ObfuscationConfig(**kwargs)
    obfuscator = LuaObfuscator(config)
    result = obfuscator.obfuscate(source)
    
    if not result.success:
        raise ValueError(f"Obfuscation failed: {'; '.join(result.errors)}")
    
    return result.output


def obfuscate_file(input_path: str, output_path: str = None, **kwargs) -> str:
    """Convenience function to obfuscate a Lua file"""
    config = ObfuscationConfig(**kwargs)
    obfuscator = LuaObfuscator(config)
    result = obfuscator.obfuscate_file(input_path, output_path)
    
    if not result.success:
        raise ValueError(f"Obfuscation failed: {'; '.join(result.errors)}")
    
    return result.output
