import random
import string
import hashlib
from typing import Dict, List, Set, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum, auto

from .parser import (
    ASTNode, Chunk, Identifier, LocalDeclaration, FunctionDecl,
    Assignment, FunctionCall, ForNumeric, ForGeneric, MemberAccess,
    IndexAccess, MethodAccess, TableConstructor, TableField,
    BinaryOp, UnaryOp, IfStmt, WhileStmt, RepeatStmt, DoBlock,
    ReturnStmt, ElseIfClause
)


class NamingStyle(Enum):
    """Variable naming styles"""
    RANDOM = auto()          # aB3xK9mN
    UNDERSCORE = auto()      # _____
    SIMILAR = auto()          # lI1O0
    HEX = auto()             # x1a2b3
    CHINESE = auto()         # Unicode chars
    MIXED = auto()           # Combination


@dataclass
class Scope:
    """Variable scope for tracking"""
    variables: Dict[str, str] = field(default_factory=dict)
    parent: Optional['Scope'] = None
    
    def lookup(self, name: str) -> Optional[str]:
        """Look up variable in scope chain"""
        if name in self.variables:
            return self.variables[name]
        if self.parent:
            return self.parent.lookup(name)
        return None
    
    def define(self, original: str, renamed: str):
        """Define variable in current scope"""
        self.variables[original] = renamed


@dataclass
class RenamerConfig:
    """Configuration for variable renaming"""
    style: NamingStyle = NamingStyle.RANDOM
    min_length: int = 4
    max_length: int = 12
    prefix: str = ""
    rename_locals: bool = True
    rename_params: bool = True
    rename_functions: bool = True
    rename_upvalues: bool = True
    preserve_globals: bool = True
    seed: int = None  # For reproducible renaming


class VariableRenamer:
    """
    Variable Renamer for Lua AST
    Renames variables to obfuscated names while preserving semantics
    """
    
    # Lua/Roblox built-in identifiers that should NOT be renamed
    RESERVED = {
        # Lua keywords
        'and', 'break', 'do', 'else', 'elseif', 'end', 'false', 'for',
        'function', 'goto', 'if', 'in', 'local', 'nil', 'not', 'or',
        'repeat', 'return', 'then', 'true', 'until', 'while', 'continue',
        
        # Special
        'self', '_G', '_ENV', '_VERSION', '...',
        
        # Lua standard library
        'print', 'error', 'warn', 'assert', 'type', 'typeof',
        'tostring', 'tonumber', 'pairs', 'ipairs', 'next', 'select',
        'unpack', 'pcall', 'xpcall', 'rawget', 'rawset', 'rawequal', 'rawlen',
        'setmetatable', 'getmetatable', 'setfenv', 'getfenv',
        'collectgarbage', 'loadstring', 'loadfile', 'dofile', 'load',
        'require', 'module', 'package',
        
        # Standard tables
        'table', 'string', 'math', 'os', 'io', 'debug', 'coroutine',
        'bit32', 'bit', 'utf8',
        
        # Roblox globals
        'game', 'workspace', 'script', 'plugin', 'shared',
        'Instance', 'Vector3', 'Vector2', 'CFrame', 'Color3', 'BrickColor',
        'UDim', 'UDim2', 'Rect', 'Region3', 'Ray', 'Faces', 'Axes',
        'TweenInfo', 'Enum', 'Random', 'NumberRange', 'NumberSequence',
        'ColorSequence', 'PhysicalProperties', 'RaycastParams',
        'OverlapParams', 'DockWidgetPluginGuiInfo', 'DateTime',
        'task', 'wait', 'Wait', 'delay', 'Delay', 'spawn', 'Spawn',
        'tick', 'time', 'elapsedTime', 'settings', 'stats', 'UserSettings',
        
        # Common service names
        'Players', 'Workspace', 'ReplicatedStorage', 'ServerStorage',
        'ServerScriptService', 'StarterGui', 'StarterPlayer', 'Teams',
        'Lighting', 'SoundService', 'Chat', 'TeleportService',
        'MarketplaceService', 'DataStoreService', 'HttpService',
        'UserInputService', 'ContextActionService', 'RunService',
        'TweenService', 'Debris', 'PathfindingService', 'PhysicsService'
    }
    
    # Confusing characters for SIMILAR style
    SIMILAR_CHARS = {
        'letters': ['l', 'I', 'i', 'L', 'j'],
        'numbers': ['1', '0', 'O', 'o'],
        'mixed': ['l', 'I', '1', 'O', '0', 'i', 'L']
    }
    
    def __init__(self, config: RenamerConfig = None):
        """
        Initialize variable renamer
        
        Args:
            config: Renaming configuration
        """
        self.config = config or RenamerConfig()
        self.name_map: Dict[str, str] = {}
        self.used_names: Set[str] = set()
        self.current_scope: Optional[Scope] = None
        self.scope_stack: List[Scope] = []
        self.counter = 0
        
        if self.config.seed is not None:
            random.seed(self.config.seed)
    
    def rename(self, ast: Chunk) -> Tuple[Chunk, Dict[str, str]]:
        """
        Rename variables in AST
        
        Args:
            ast: Parsed Lua AST
            
        Returns:
            Tuple of (modified AST, name mapping)
        """
        self.name_map = {}
        self.used_names = set(self.RESERVED)
        self.current_scope = Scope()
        self.scope_stack = [self.current_scope]
        self.counter = 0
        
        # First pass: collect all variable declarations
        self._collect_declarations(ast)
        
        # Second pass: rename all references
        self._rename_references(ast)
        
        return ast, self.name_map.copy()
    
    def _enter_scope(self):
        """Enter a new scope"""
        new_scope = Scope(parent=self.current_scope)
        self.scope_stack.append(new_scope)
        self.current_scope = new_scope
    
    def _leave_scope(self):
        """Leave current scope"""
        if len(self.scope_stack) > 1:
            self.scope_stack.pop()
            self.current_scope = self.scope_stack[-1]
    
    def _generate_name(self) -> str:
        """Generate a new obfuscated name"""
        style = self.config.style
        prefix = self.config.prefix
        
        while True:
            self.counter += 1
            
            if style == NamingStyle.RANDOM:
                name = self._generate_random_name()
            elif style == NamingStyle.UNDERSCORE:
                name = self._generate_underscore_name()
            elif style == NamingStyle.SIMILAR:
                name = self._generate_similar_name()
            elif style == NamingStyle.HEX:
                name = self._generate_hex_name()
            elif style == NamingStyle.CHINESE:
                name = self._generate_unicode_name()
            elif style == NamingStyle.MIXED:
                name = self._generate_mixed_name()
            else:
                name = self._generate_random_name()
            
            full_name = prefix + name
            
            if full_name not in self.used_names and full_name not in self.RESERVED:
                self.used_names.add(full_name)
                return full_name
    
    def _generate_random_name(self) -> str:
        """Generate random alphanumeric name"""
        length = random.randint(self.config.min_length, self.config.max_length)
        chars = string.ascii_letters + '_'
        first = random.choice(string.ascii_letters + '_')
        rest = ''.join(random.choice(chars + string.digits) for _ in range(length - 1))
        return first + rest
    
    def _generate_underscore_name(self) -> str:
        """Generate underscore-based name"""
        length = random.randint(self.config.min_length, self.config.max_length)
        # Mix of underscores and l/I characters
        chars = ['_', 'l', 'I', '_', '_']
        first = '_'
        rest = ''.join(random.choice(chars) for _ in range(length - 1))
        return first + rest + str(self.counter)
    
    def _generate_similar_name(self) -> str:
        """Generate name with similar-looking characters"""
        length = random.randint(self.config.min_length, self.config.max_length)
        # Start with letter
        first = random.choice(['l', 'I', 'L', 'i'])
        rest = ''.join(random.choice(self.SIMILAR_CHARS['mixed']) 
                      for _ in range(length - 1))
        # Add counter to ensure uniqueness
        return first + rest + format(self.counter, 'x')
    
    def _generate_hex_name(self) -> str:
        """Generate hex-like name"""
        # Generate hash-based name
        hash_input = f"{self.counter}_{random.random()}"
        hash_val = hashlib.md5(hash_input.encode()).hexdigest()
        length = random.randint(self.config.min_length, self.config.max_length)
        return '_x' + hash_val[:length-2]
    
    def _generate_unicode_name(self) -> str:
        """Generate name with Unicode characters (for Luau)"""
        # Lua 5.3+ and Luau support Unicode identifiers
        # Using Chinese characters that look similar
        unicode_chars = [
            '\u4e00', '\u4e01', '\u4e02', '\u4e03',  # Chinese
            '\u0430', '\u0435', '\u043e', '\u0440',  # Cyrillic (looks like latin)
        ]
        first = random.choice(string.ascii_letters)
        length = random.randint(2, 4)
        rest = ''.join(random.choice(unicode_chars) for _ in range(length))
        return first + rest + str(self.counter)
    
    def _generate_mixed_name(self) -> str:
        """Generate name using mixed styles"""
        styles = [
            self._generate_random_name,
            self._generate_underscore_name,
            self._generate_hex_name
        ]
        return random.choice(styles)()
    
    def _should_rename(self, name: str, is_global: bool = False) -> bool:
        """Check if variable should be renamed"""
        if name in self.RESERVED:
            return False
        
        if is_global and self.config.preserve_globals:
            return False
        
        return True
    
    def _get_or_create_name(self, original: str, force_new: bool = False) -> str:
        """Get existing renamed name or create new one"""
        if not self._should_rename(original):
            return original
        
        # Check current scope chain
        renamed = self.current_scope.lookup(original)
        if renamed and not force_new:
            return renamed
        
        # Generate new name
        new_name = self._generate_name()
        self.current_scope.define(original, new_name)
        self.name_map[original] = new_name
        
        return new_name
    
    def _collect_declarations(self, node: ASTNode):
        """Collect all variable declarations (first pass)"""
        if node is None:
            return
        
        if isinstance(node, Chunk):
            for stmt in node.body:
                self._collect_declarations(stmt)
        
        elif isinstance(node, LocalDeclaration):
            if self.config.rename_locals:
                for name in node.names:
                    if self._should_rename(name):
                        self._get_or_create_name(name, force_new=True)
            
            for value in node.values:
                self._collect_declarations(value)
        
        elif isinstance(node, FunctionDecl):
            # Function name (if local)
            if node.is_local and node.name and self.config.rename_functions:
                if isinstance(node.name, Identifier):
                    if self._should_rename(node.name.name):
                        self._get_or_create_name(node.name.name, force_new=True)
            
            # Enter function scope
            self._enter_scope()
            
            # Parameters
            if self.config.rename_params:
                for param in node.params:
                    if self._should_rename(param):
                        self._get_or_create_name(param, force_new=True)
            
            # Body
            for stmt in node.body:
                self._collect_declarations(stmt)
            
            self._leave_scope()
        
        elif isinstance(node, IfStmt):
            self._collect_declarations(node.condition)
            
            self._enter_scope()
            for stmt in node.then_body:
                self._collect_declarations(stmt)
            self._leave_scope()
            
            for clause in node.elseif_clauses:
                self._collect_declarations(clause.condition)
                self._enter_scope()
                for stmt in clause.body:
                    self._collect_declarations(stmt)
                self._leave_scope()
            
            if node.else_body:
                self._enter_scope()
                for stmt in node.else_body:
                    self._collect_declarations(stmt)
                self._leave_scope()
        
        elif isinstance(node, WhileStmt):
            self._collect_declarations(node.condition)
            self._enter_scope()
            for stmt in node.body:
                self._collect_declarations(stmt)
            self._leave_scope()
        
        elif isinstance(node, ForNumeric):
            self._enter_scope()
            if self._should_rename(node.var) and self.config.rename_locals:
                self._get_or_create_name(node.var, force_new=True)
            
            self._collect_declarations(node.start)
            self._collect_declarations(node.stop)
            if node.step:
                self._collect_declarations(node.step)
            
            for stmt in node.body:
                self._collect_declarations(stmt)
            self._leave_scope()
        
        elif isinstance(node, ForGeneric):
            self._enter_scope()
            if self.config.rename_locals:
                for var in node.vars:
                    if self._should_rename(var):
                        self._get_or_create_name(var, force_new=True)
            
            for iterator in node.iterators:
                self._collect_declarations(iterator)
            
            for stmt in node.body:
                self._collect_declarations(stmt)
            self._leave_scope()
        
        elif isinstance(node, RepeatStmt):
            self._enter_scope()
            for stmt in node.body:
                self._collect_declarations(stmt)
            self._collect_declarations(node.condition)
            self._leave_scope()
        
        elif isinstance(node, DoBlock):
            self._enter_scope()
            for stmt in node.body:
                self._collect_declarations(stmt)
            self._leave_scope()
        
        elif isinstance(node, (BinaryOp, UnaryOp)):
            if hasattr(node, 'left'):
                self._collect_declarations(node.left)
            if hasattr(node, 'right'):
                self._collect_declarations(node.right)
            if hasattr(node, 'operand'):
                self._collect_declarations(node.operand)
        
        elif isinstance(node, FunctionCall):
            self._collect_declarations(node.callee)
            for arg in node.args:
                self._collect_declarations(arg)
        
        elif isinstance(node, TableConstructor):
            for field in node.fields:
                if field.key:
                    self._collect_declarations(field.key)
                self._collect_declarations(field.value)
        
        elif isinstance(node, (IndexAccess, MemberAccess)):
            self._collect_declarations(node.object)
            if isinstance(node, IndexAccess):
                self._collect_declarations(node.index)
        
        elif isinstance(node, Assignment):
            for target in node.targets:
                self._collect_declarations(target)
            for value in node.values:
                self._collect_declarations(value)
        
        elif isinstance(node, ReturnStmt):
            for value in node.values:
                self._collect_declarations(value)
    
    def _rename_references(self, node: ASTNode):
        """Rename all variable references (second pass)"""
        if node is None:
            return
        
        if isinstance(node, Chunk):
            for stmt in node.body:
                self._rename_references(stmt)
        
        elif isinstance(node, Identifier):
            renamed = self.current_scope.lookup(node.name)
            if renamed:
                node.name = renamed
        
        elif isinstance(node, LocalDeclaration):
            # Rename variable names
            for i, name in enumerate(node.names):
                renamed = self.current_scope.lookup(name)
                if renamed:
                    node.names[i] = renamed
            
            # Rename in values
            for value in node.values:
                self._rename_references(value)
        
        elif isinstance(node, FunctionDecl):
            # Rename function name if local
            if node.is_local and node.name:
                if isinstance(node.name, Identifier):
                    renamed = self.current_scope.lookup(node.name.name)
                    if renamed:
                        node.name.name = renamed
            
            # Enter function scope
            self._enter_scope()
            
            # Rename parameters
            for i, param in enumerate(node.params):
                renamed = self.current_scope.lookup(param)
                if renamed:
                    node.params[i] = renamed
            
            # Rename body
            for stmt in node.body:
                self._rename_references(stmt)
            
            self._leave_scope()
        
        elif isinstance(node, IfStmt):
            self._rename_references(node.condition)
            
            self._enter_scope()
            for stmt in node.then_body:
                self._rename_references(stmt)
            self._leave_scope()
            
            for clause in node.elseif_clauses:
                self._rename_references(clause.condition)
                self._enter_scope()
                for stmt in clause.body:
                    self._rename_references(stmt)
                self._leave_scope()
            
            if node.else_body:
                self._enter_scope()
                for stmt in node.else_body:
                    self._rename_references(stmt)
                self._leave_scope()
        
        elif isinstance(node, WhileStmt):
            self._rename_references(node.condition)
            self._enter_scope()
            for stmt in node.body:
                self._rename_references(stmt)
            self._leave_scope()
        
        elif isinstance(node, ForNumeric):
            self._enter_scope()
            
            # Rename loop variable
            renamed = self.current_scope.lookup(node.var)
            if renamed:
                node.var = renamed
            
            self._rename_references(node.start)
            self._rename_references(node.stop)
            if node.step:
                self._rename_references(node.step)
            
            for stmt in node.body:
                self._rename_references(stmt)
            self._leave_scope()
        
        elif isinstance(node, ForGeneric):
            self._enter_scope()
            
            # Rename loop variables
            for i, var in enumerate(node.vars):
                renamed = self.current_scope.lookup(var)
                if renamed:
                    node.vars[i] = renamed
            
            for iterator in node.iterators:
                self._rename_references(iterator)
            
            for stmt in node.body:
                self._rename_references(stmt)
            self._leave_scope()
        
        elif isinstance(node, RepeatStmt):
            self._enter_scope()
            for stmt in node.body:
                self._rename_references(stmt)
            self._rename_references(node.condition)
            self._leave_scope()
        
        elif isinstance(node, DoBlock):
            self._enter_scope()
            for stmt in node.body:
                self._rename_references(stmt)
            self._leave_scope()
        
        elif isinstance(node, BinaryOp):
            self._rename_references(node.left)
            self._rename_references(node.right)
        
        elif isinstance(node, UnaryOp):
            self._rename_references(node.operand)
        
        elif isinstance(node, FunctionCall):
            self._rename_references(node.callee)
            for arg in node.args:
                self._rename_references(arg)
        
        elif isinstance(node, IndexAccess):
            self._rename_references(node.object)
            self._rename_references(node.index)
        
        elif isinstance(node, MemberAccess):
            self._rename_references(node.object)
            # Don't rename member names (they're string keys)
        
        elif isinstance(node, TableConstructor):
            for field in node.fields:
                # Don't rename string keys
                if field.key and not isinstance(field.key, str):
                    self._rename_references(field.key)
                self._rename_references(field.value)
        
        elif isinstance(node, Assignment):
            for target in node.targets:
                self._rename_references(target)
            for value in node.values:
                self._rename_references(value)
        
        elif isinstance(node, ReturnStmt):
            for value in node.values:
                self._rename_references(value)


def rename_variables(ast: Chunk, style: NamingStyle = NamingStyle.RANDOM,
                    **kwargs) -> Tuple[Chunk, Dict[str, str]]:
    """
    Convenience function to rename variables in AST
    
    Args:
        ast: Parsed Lua AST
        style: Naming style to use
        **kwargs: Additional config options
        
    Returns:
        Tuple of (modified AST, name mapping)
    """
    config = RenamerConfig(style=style, **kwargs)
    renamer = VariableRenamer(config)
    return renamer.rename(ast)
