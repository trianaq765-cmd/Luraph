import random
import time
from typing import Dict, List, Any
from dataclasses import dataclass, field

from .lexer import LuaLexer
from .parser import LuaParser
from .compiler import BytecodeCompiler, CompiledFunction
from .vm_generator import VMGenerator


@dataclass
class ObfuscationConfig:
    use_vm: bool = True
    encrypt_strings: bool = True
    encrypt_bytecode: bool = True
    encryption_method: str = 'xor'
    rename_variables: bool = True
    add_junk_code: bool = True
    junk_code_ratio: float = 0.3
    add_anti_tamper: bool = True
    add_anti_dump: bool = True
    mode: str = 'loadstring'
    minify: bool = True
    add_watermark: bool = True
    watermark_text: str = "Protected by LuaShield"
    target_ratio: float = 6.0  # Target 600% output ratio


@dataclass
class ObfuscationResult:
    success: bool
    output: str
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    stats: Dict[str, Any] = field(default_factory=dict)


class LuaObfuscator:
    """Professional Lua Obfuscator - Dense Output"""
    
    def __init__(self, config: ObfuscationConfig = None):
        self.config = config or ObfuscationConfig()
        self.errors: List[str] = []
        self.warnings: List[str] = []
        self.stats: Dict[str, Any] = {}
    
    def obfuscate(self, source: str) -> ObfuscationResult:
        start_time = time.time()
        self.errors = []
        self.warnings = []
        self.stats = {
            'original_size': len(source),
            'original_lines': source.count('\n') + 1
        }
        
        try:
            if not self._validate(source):
                return ObfuscationResult(success=False, output='', errors=self.errors)
            
            lexer = LuaLexer(source)
            tokens = lexer.tokenize()
            if lexer.get_errors():
                self.warnings.extend(lexer.get_errors())
            
            parser = LuaParser(tokens)
            ast = parser.parse()
            if parser.get_errors():
                self.warnings.extend(parser.get_errors())
            
            compiler = BytecodeCompiler()
            bytecode, errs = compiler.compile(ast)
            if errs:
                self.warnings.extend(errs)
            
            self.stats['constants'] = len(bytecode.constants)
            self.stats['instructions'] = len(bytecode.instructions)
            self.stats['functions'] = len(bytecode.children) + 1
            
            output = self._generate(bytecode)
            
            self.stats['output_size'] = len(output)
            self.stats['output_lines'] = output.count('\n') + 1
            self.stats['time_ms'] = int((time.time() - start_time) * 1000)
            self.stats['compression_ratio'] = round(len(output) / len(source) * 100, 2) if source else 0
            
            return ObfuscationResult(
                success=True, output=output,
                errors=self.errors, warnings=self.warnings, stats=self.stats
            )
        except Exception as e:
            self.errors.append(f"Failed: {e}")
            return ObfuscationResult(success=False, output='', errors=self.errors)
    
    def _validate(self, source: str) -> bool:
        if not source or not source.strip():
            self.errors.append("Empty source")
            return False
        if source.startswith('\x1bLua'):
            self.errors.append("Luac not supported")
            return False
        return True
    
    def _generate(self, bytecode: CompiledFunction) -> str:
        key = bytes([random.randint(0, 255) for _ in range(32)])
        vm_gen = VMGenerator(key=key, use_encryption=self.config.encrypt_bytecode)
        return vm_gen.generate(bytecode, mode=self.config.mode)
    
    def obfuscate_file(self, input_path: str, output_path: str = None) -> ObfuscationResult:
        if input_path.endswith('.luac'):
            return ObfuscationResult(success=False, output='', errors=["Luac not supported"])
        
        try:
            with open(input_path, 'r', encoding='utf-8') as f:
                source = f.read()
        except Exception as e:
            return ObfuscationResult(success=False, output='', errors=[str(e)])
        
        result = self.obfuscate(source)
        
        if result.success and output_path:
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(result.output)
        
        return result


def obfuscate(source: str, **kwargs) -> str:
    result = LuaObfuscator(ObfuscationConfig(**kwargs)).obfuscate(source)
    if not result.success:
        raise ValueError('; '.join(result.errors))
    return result.output
