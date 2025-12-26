import random
import string
import math
from typing import List, Dict, Any, Optional, Callable
from dataclasses import dataclass, field
from enum import Enum, auto


class JunkType(Enum):
    """Types of junk code"""
    DEAD_VARIABLE = auto()      # Unused variable declarations
    DEAD_FUNCTION = auto()      # Unused function declarations
    DEAD_BRANCH = auto()        # if false then ... end
    OPAQUE_PREDICATE = auto()   # Complex conditions that always true/false
    FAKE_LOOP = auto()          # Loops that execute 0 or 1 times
    MATH_IDENTITY = auto()      # x = x + 0, x = x * 1
    STRING_OPERATIONS = auto()  # Unused string operations
    TABLE_OPERATIONS = auto()   # Unused table operations
    COMMENT_NOISE = auto()      # Fake comments
    CONTROL_FLOW = auto()       # Fake control flow


@dataclass
class JunkConfig:
    """Configuration for junk code generation"""
    enabled_types: List[JunkType] = field(default_factory=lambda: list(JunkType))
    intensity: float = 0.3      # 0.0 to 1.0
    min_junk_per_block: int = 1
    max_junk_per_block: int = 5
    variable_prefix: str = "_"
    use_complex_expressions: bool = True
    seed: int = None


class JunkCodeGenerator:
    """
    Junk/Dead Code Generator
    Generates various types of junk code to confuse deobfuscators
    """
    
    def __init__(self, config: JunkConfig = None):
        """
        Initialize junk code generator
        
        Args:
            config: Generation configuration
        """
        self.config = config or JunkConfig()
        self.counter = 0
        self.generated_names: set = set()
        
        if self.config.seed is not None:
            random.seed(self.config.seed)
        
        # Generator functions for each type
        self.generators: Dict[JunkType, Callable] = {
            JunkType.DEAD_VARIABLE: self._gen_dead_variable,
            JunkType.DEAD_FUNCTION: self._gen_dead_function,
            JunkType.DEAD_BRANCH: self._gen_dead_branch,
            JunkType.OPAQUE_PREDICATE: self._gen_opaque_predicate,
            JunkType.FAKE_LOOP: self._gen_fake_loop,
            JunkType.MATH_IDENTITY: self._gen_math_identity,
            JunkType.STRING_OPERATIONS: self._gen_string_operation,
            JunkType.TABLE_OPERATIONS: self._gen_table_operation,
            JunkType.COMMENT_NOISE: self._gen_comment_noise,
            JunkType.CONTROL_FLOW: self._gen_control_flow
        }
    
    def _random_name(self, length: int = 6) -> str:
        """Generate random variable name"""
        self.counter += 1
        prefix = self.config.variable_prefix
        chars = string.ascii_letters + '_'
        first = random.choice(string.ascii_letters + '_')
        rest = ''.join(random.choice(chars + string.digits) for _ in range(length - 1))
        name = f"{prefix}{first}{rest}{self.counter}"
        self.generated_names.add(name)
        return name
    
    def _random_int(self, min_val: int = 0, max_val: int = 99999) -> int:
        """Generate random integer"""
        return random.randint(min_val, max_val)
    
    def _random_float(self) -> float:
        """Generate random float"""
        return round(random.uniform(-1000, 1000), random.randint(1, 6))
    
    def _random_string(self, length: int = None) -> str:
        """Generate random string literal"""
        if length is None:
            length = random.randint(4, 16)
        chars = string.ascii_letters + string.digits + '_'
        return ''.join(random.choice(chars) for _ in range(length))
    
    def _random_operator(self) -> str:
        """Get random arithmetic operator"""
        return random.choice(['+', '-', '*', '/', '%'])
    
    def _random_comparison(self) -> str:
        """Get random comparison operator"""
        return random.choice(['==', '~=', '<', '>', '<=', '>='])
    
    def generate(self, count: int = None) -> List[str]:
        """
        Generate junk code lines
        
        Args:
            count: Number of junk items (random if None)
            
        Returns:
            List of junk code lines
        """
        if count is None:
            count = random.randint(
                self.config.min_junk_per_block,
                self.config.max_junk_per_block
            )
        
        enabled = self.config.enabled_types
        if not enabled:
            enabled = list(JunkType)
        
        result = []
        for _ in range(count):
            junk_type = random.choice(enabled)
            generator = self.generators.get(junk_type, self._gen_dead_variable)
            
            try:
                junk = generator()
                if junk:
                    if isinstance(junk, list):
                        result.extend(junk)
                    else:
                        result.append(junk)
            except Exception:
                # Fallback to simple junk
                result.append(self._gen_dead_variable())
        
        return result
    
    def generate_block(self, min_lines: int = 3, max_lines: int = 10) -> str:
        """Generate a block of junk code"""
        count = random.randint(min_lines, max_lines)
        lines = self.generate(count)
        return '\n'.join(lines)
    
    def _gen_dead_variable(self) -> str:
        """Generate dead variable declaration"""
        name = self._random_name()
        value_type = random.choice(['int', 'float', 'string', 'bool', 'nil', 'table', 'expr'])
        
        if value_type == 'int':
            value = str(self._random_int())
        elif value_type == 'float':
            value = str(self._random_float())
        elif value_type == 'string':
            value = f'"{self._random_string()}"'
        elif value_type == 'bool':
            value = random.choice(['true', 'false'])
        elif value_type == 'nil':
            value = 'nil'
        elif value_type == 'table':
            value = '{}'
        else:
            value = self._gen_complex_expression()
        
        return f"local {name} = {value}"
    
    def _gen_dead_function(self) -> List[str]:
        """Generate dead function declaration"""
        name = self._random_name()
        params = ', '.join(self._random_name(4) for _ in range(random.randint(0, 3)))
        
        body_lines = []
        for _ in range(random.randint(1, 4)):
            body_lines.append(f"    {self._gen_dead_variable()}")
        
        if random.random() > 0.5:
            body_lines.append(f"    return {self._random_int()}")
        
        lines = [
            f"local function {name}({params})",
            *body_lines,
            "end"
        ]
        
        return lines
    
    def _gen_dead_branch(self) -> List[str]:
        """Generate dead branch (never executed)"""
        condition = random.choice([
            'false',
            'nil',
            f'{self._random_int()} > {self._random_int() + 10000}',
            f'"{self._random_string()}" == "{self._random_string()}"',
            f'type(nil) == "string"',
            f'{self._random_int()} < 0 and {self._random_int()} > 0'
        ])
        
        body = self._gen_dead_variable()
        
        return [
            f"if {condition} then",
            f"    {body}",
            "end"
        ]
    
    def _gen_opaque_predicate(self) -> List[str]:
        """Generate opaque predicate (always true/false but hard to analyze)"""
        x = self._random_int(1, 100)
        
        # Mathematical identities that are always true
        always_true = [
            f"({x} * {x} >= 0)",
            f"({x} + {x} == {x * 2})",
            f"(({x} % 2 == 0) or ({x} % 2 == 1))",
            f"({x} * 0 == 0)",
            f"(type({x}) == 'number')",
            f"(#{{{x}}} == 1)",
            f"(math.abs({x}) >= 0)",
            f"(({x} // 1) == {x})",
        ]
        
        condition = random.choice(always_true)
        inner = self._gen_dead_variable()
        
        return [
            f"if {condition} then",
            f"    {inner}",
            "end"
        ]
    
    def _gen_fake_loop(self) -> List[str]:
        """Generate fake loop (executes 0 or 1 times)"""
        var = self._random_name(4)
        loop_type = random.choice(['for_zero', 'for_one', 'while_false'])
        
        if loop_type == 'for_zero':
            return [
                f"for {var} = 1, 0 do",
                f"    local _ = {var}",
                "end"
            ]
        elif loop_type == 'for_one':
            return [
                f"for {var} = 1, 1 do",
                f"    local _ = {var} * 0",
                "end"
            ]
        else:  # while_false
            return [
                f"while false do",
                f"    break",
                "end"
            ]
    
    def _gen_math_identity(self) -> str:
        """Generate mathematical identity operation"""
        name = self._random_name()
        init = self._random_int()
        
        identities = [
            f"local {name} = {init} + 0",
            f"local {name} = {init} * 1",
            f"local {name} = {init} - 0",
            f"local {name} = {init} / 1",
            f"local {name} = {init} ^ 1",
            f"local {name} = {init} % {init + self._random_int(1, 100)}",
            f"local {name} = math.floor({init}.0)",
            f"local {name} = math.ceil({init}.0)",
            f"local {name} = math.abs({init})",
            f"local {name} = {init} * 2 / 2",
            f"local {name} = ({init} + {init}) - {init}",
        ]
        
        return random.choice(identities)
    
    def _gen_string_operation(self) -> str:
        """Generate unused string operation"""
        name = self._random_name()
        s = self._random_string()
        
        operations = [
            f'local {name} = string.len("{s}")',
            f'local {name} = string.reverse("{s}")',
            f'local {name} = string.lower("{s}")',
            f'local {name} = string.upper("{s}")',
            f'local {name} = string.sub("{s}", 1, 1)',
            f'local {name} = string.rep("{s}", 1)',
            f'local {name} = string.byte("{s}", 1)',
            f'local {name} = string.char({self._random_int(65, 90)})',
            f'local {name} = "{s}" .. ""',
            f'local {name} = #{s}',
        ]
        
        return random.choice(operations)
    
    def _gen_table_operation(self) -> List[str]:
        """Generate unused table operation"""
        name = self._random_name()
        key = self._random_name(4)
        
        operations = [
            [f"local {name} = {{}}", f"{name}['{key}'] = {self._random_int()}"],
            [f"local {name} = {{{self._random_int()}, {self._random_int()}}}"],
            [f"local {name} = {{['{key}'] = {self._random_int()}}}"],
            [f"local {name} = table.pack({self._random_int()})"],
            [f"local {name} = {{}}; {name}.{key} = nil"],
        ]
        
        return random.choice(operations)
    
    def _gen_comment_noise(self) -> str:
        """Generate fake/noise comments"""
        comments = [
            f"-- TODO: {self._random_string(12)}",
            f"-- FIXME: {self._random_string(10)}",
            f"-- NOTE: {self._random_string(15)}",
            f"-- {self._random_string(20)}",
            f"--[[ {self._random_string(25)} ]]",
            f"-- v{self._random_int(1, 9)}.{self._random_int(0, 99)}",
            f"-- Line {self._random_int(1, 999)}",
            f"-- Debug: 0x{format(self._random_int(0, 65535), 'x')}",
        ]
        
        return random.choice(comments)
    
    def _gen_control_flow(self) -> List[str]:
        """Generate fake control flow"""
        var = self._random_name()
        
        patterns = [
            # Redundant do-end
            [
                "do",
                f"    local {var} = {self._random_int()}",
                "end"
            ],
            # Empty repeat-until
            [
                "repeat",
                f"    local {var} = nil",
                "until true"
            ],
            # Redundant and/or
            [
                f"local {var} = true and {self._random_int()}",
            ],
            [
                f"local {var} = false or {self._random_int()}",
            ],
        ]
        
        return random.choice(patterns)
    
    def _gen_complex_expression(self) -> str:
        """Generate complex but evaluable expression"""
        if not self.config.use_complex_expressions:
            return str(self._random_int())
        
        a, b, c = self._random_int(1, 100), self._random_int(1, 100), self._random_int(1, 100)
        
        expressions = [
            f"(({a} + {b}) * {c} - {a * b})",
            f"math.floor(({a} + {b}) / 2)",
            f"math.max({a}, {b}, {c})",
            f"math.min({a}, {b}) + math.max({a}, {c})",
            f"({a} * {b} % {c + 1})",
            f"(({a} > {b}) and {a} or {b})",
            f"#{{{a}, {b}, {c}}}",
            f"bit32.band({a}, {b})" if random.random() > 0.5 else str(a & b),
        ]
        
        return random.choice(expressions)
    
    def inject_into_code(self, code: str, positions: str = 'random') -> str:
        """
        Inject junk code into existing Lua code
        
        Args:
            code: Original Lua code
            positions: 'random', 'start', 'end', 'between'
            
        Returns:
            Code with injected junk
        """
        lines = code.split('\n')
        result = []
        
        intensity = self.config.intensity
        
        for i, line in enumerate(lines):
            # Add junk before line based on probability
            if positions in ['random', 'between']:
                if random.random() < intensity:
                    if not self._is_block_boundary(line):
                        junk = self.generate(random.randint(1, 2))
                        result.extend(junk)
            
            result.append(line)
        
        # Add junk at start
        if positions in ['start', 'random']:
            start_junk = self.generate(random.randint(2, 4))
            result = start_junk + result
        
        # Add junk at end
        if positions in ['end', 'random']:
            end_junk = self.generate(random.randint(2, 4))
            result.extend(end_junk)
        
        return '\n'.join(result)
    
    def _is_block_boundary(self, line: str) -> bool:
        """Check if line is a block boundary (shouldn't insert junk after)"""
        stripped = line.strip()
        boundaries = [
            'function', 'then', 'do', 'else', 'elseif', 'repeat',
            'end', 'until', '{', '}', 'return', '--'
        ]
        
        for boundary in boundaries:
            if stripped.startswith(boundary) or stripped.endswith(boundary):
                return True
        
        return False


def generate_junk(count: int = 5, **kwargs) -> List[str]:
    """
    Convenience function to generate junk code
    
    Args:
        count: Number of junk lines
        **kwargs: JunkConfig options
        
    Returns:
        List of junk code lines
    """
    config = JunkConfig(**kwargs)
    generator = JunkCodeGenerator(config)
    return generator.generate(count)


def inject_junk(code: str, intensity: float = 0.3, **kwargs) -> str:
    """
    Convenience function to inject junk into code
    
    Args:
        code: Original Lua code
        intensity: Junk code intensity (0.0 to 1.0)
        **kwargs: JunkConfig options
        
    Returns:
        Code with injected junk
    """
    config = JunkConfig(intensity=intensity, **kwargs)
    generator = JunkCodeGenerator(config)
    return generator.inject_into_code(code)
