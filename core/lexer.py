import re
from enum import Enum, auto
from dataclasses import dataclass
from typing import List, Optional, Tuple

class TokenType(Enum):
    # Literals
    NUMBER = auto()
    STRING = auto()
    BOOLEAN = auto()
    NIL = auto()
    IDENTIFIER = auto()
    
    # Keywords
    AND = auto()
    BREAK = auto()
    DO = auto()
    ELSE = auto()
    ELSEIF = auto()
    END = auto()
    FALSE = auto()
    FOR = auto()
    FUNCTION = auto()
    GOTO = auto()
    IF = auto()
    IN = auto()
    LOCAL = auto()
    NOT = auto()
    OR = auto()
    REPEAT = auto()
    RETURN = auto()
    THEN = auto()
    TRUE = auto()
    UNTIL = auto()
    WHILE = auto()
    CONTINUE = auto()  # Roblox extension
    
    # Operators
    PLUS = auto()
    MINUS = auto()
    STAR = auto()
    SLASH = auto()
    PERCENT = auto()
    CARET = auto()
    HASH = auto()
    EQ = auto()
    NEQ = auto()
    LTE = auto()
    GTE = auto()
    LT = auto()
    GT = auto()
    ASSIGN = auto()
    CONCAT = auto()
    VARARG = auto()
    DOUBLE_COLON = auto()
    PLUS_EQ = auto()    # Roblox +=
    MINUS_EQ = auto()   # Roblox -=
    STAR_EQ = auto()    # Roblox *=
    SLASH_EQ = auto()   # Roblox /=
    
    # Delimiters
    LPAREN = auto()
    RPAREN = auto()
    LBRACE = auto()
    RBRACE = auto()
    LBRACKET = auto()
    RBRACKET = auto()
    SEMICOLON = auto()
    COLON = auto()
    COMMA = auto()
    DOT = auto()
    
    # Special
    EOF = auto()

@dataclass
class Token:
    type: TokenType
    value: any
    line: int
    column: int
    raw: str = ""  # Original text
    
    def __repr__(self):
        return f"Token({self.type.name}, {repr(self.value)}, L{self.line}:C{self.column})"

class LuaLexer:
    """
    Lua Lexer - Tokenizes Lua/Roblox source code
    Supports: .lua files, .txt files with Lua code, raw Lua strings
    Does NOT support: .luac bytecode files
    """
    
    KEYWORDS = {
        'and': TokenType.AND, 'break': TokenType.BREAK, 'do': TokenType.DO,
        'else': TokenType.ELSE, 'elseif': TokenType.ELSEIF, 'end': TokenType.END,
        'false': TokenType.FALSE, 'for': TokenType.FOR, 'function': TokenType.FUNCTION,
        'goto': TokenType.GOTO, 'if': TokenType.IF, 'in': TokenType.IN,
        'local': TokenType.LOCAL, 'nil': TokenType.NIL, 'not': TokenType.NOT,
        'or': TokenType.OR, 'repeat': TokenType.REPEAT, 'return': TokenType.RETURN,
        'then': TokenType.THEN, 'true': TokenType.TRUE, 'until': TokenType.UNTIL,
        'while': TokenType.WHILE, 'continue': TokenType.CONTINUE
    }
    
    def __init__(self, source: str):
        """
        Initialize lexer with Lua source code string
        
        Args:
            source: Raw Lua source code as string (from .lua or .txt file)
        """
        # Validate input is text, not bytecode
        if source.startswith('\x1bLua') or source.startswith('\x1b\x4c\x75\x61'):
            raise ValueError("Luac bytecode is not supported. Please provide Lua source code (.lua or .txt)")
        
        # Normalize line endings
        self.source = source.replace('\r\n', '\n').replace('\r', '\n')
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
        self.errors: List[str] = []
        
    @classmethod
    def from_file(cls, filepath: str) -> 'LuaLexer':
        """
        Create lexer from file path
        Supports .lua and .txt files
        
        Args:
            filepath: Path to .lua or .txt file
        """
        if filepath.endswith('.luac'):
            raise ValueError("Luac bytecode files are not supported. Please use .lua or .txt files")
        
        with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
            source = f.read()
        
        return cls(source)
    
    @classmethod
    def from_string(cls, source: str) -> 'LuaLexer':
        """
        Create lexer from string
        
        Args:
            source: Lua source code as string
        """
        return cls(source)
    
    def peek(self, offset: int = 0) -> Optional[str]:
        """Look ahead in source without consuming"""
        pos = self.pos + offset
        if pos < len(self.source):
            return self.source[pos]
        return None
    
    def peek_string(self, length: int) -> str:
        """Peek multiple characters"""
        return self.source[self.pos:self.pos + length]
    
    def advance(self) -> Optional[str]:
        """Consume and return next character"""
        if self.pos < len(self.source):
            char = self.source[self.pos]
            self.pos += 1
            if char == '\n':
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            return char
        return None
    
    def skip_whitespace(self):
        """Skip spaces, tabs, and newlines"""
        while self.peek() and self.peek() in ' \t\n':
            self.advance()
    
    def skip_comment(self) -> bool:
        """Skip single-line and multi-line comments"""
        if self.peek() == '-' and self.peek(1) == '-':
            self.advance()  # -
            self.advance()  # -
            
            # Check for long comment --[[...]] or --[=[...]=]
            if self.peek() == '[':
                level = self.get_long_bracket_level()
                if level >= 0:
                    self.skip_long_string(level)
                    return True
            
            # Single line comment
            while self.peek() and self.peek() != '\n':
                self.advance()
            return True
        return False
    
    def get_long_bracket_level(self) -> int:
        """
        Check for long bracket and return level
        [[ = level 0, [=[ = level 1, [==[ = level 2, etc.
        Returns -1 if not a long bracket
        """
        if self.peek() != '[':
            return -1
        
        pos = self.pos + 1
        level = 0
        
        while pos < len(self.source) and self.source[pos] == '=':
            level += 1
            pos += 1
        
        if pos < len(self.source) and self.source[pos] == '[':
            return level
        
        return -1
    
    def skip_long_string(self, level: int):
        """Skip long string/comment content"""
        # Skip opening bracket [=*[
        self.advance()  # [
        for _ in range(level):
            self.advance()  # =
        self.advance()  # [
        
        # Find closing bracket ]=*]
        closing = ']' + ('=' * level) + ']'
        
        while self.pos < len(self.source):
            if self.peek_string(len(closing)) == closing:
                for _ in range(len(closing)):
                    self.advance()
                return
            self.advance()
    
    def read_long_string(self) -> Token:
        """Read long string literal [[...]] or [=[...]=]"""
        start_line, start_col = self.line, self.column
        level = self.get_long_bracket_level()
        
        # Skip opening bracket
        self.advance()  # [
        for _ in range(level):
            self.advance()  # =
        self.advance()  # [
        
        # Skip initial newline if present
        if self.peek() == '\n':
            self.advance()
        
        value = ""
        closing = ']' + ('=' * level) + ']'
        
        while self.pos < len(self.source):
            if self.peek_string(len(closing)) == closing:
                for _ in range(len(closing)):
                    self.advance()
                return Token(TokenType.STRING, value, start_line, start_col)
            
            char = self.advance()
            if char:
                value += char
        
        self.errors.append(f"Unterminated long string at L{start_line}:C{start_col}")
        return Token(TokenType.STRING, value, start_line, start_col)
    
    def read_string(self) -> Token:
        """Read quoted string literal"""
        start_line, start_col = self.line, self.column
        quote = self.advance()  # ' or "
        value = ""
        raw = quote
        
        while self.peek() and self.peek() != quote:
            char = self.advance()
            raw += char
            
            if char == '\\':
                escape_char = self.advance()
                if escape_char:
                    raw += escape_char
                    escape_map = {
                        'a': '\a', 'b': '\b', 'f': '\f', 'n': '\n',
                        'r': '\r', 't': '\t', 'v': '\v', '\\': '\\',
                        '"': '"', "'": "'", '\n': '\n', '0': '\0'
                    }
                    
                    if escape_char in escape_map:
                        value += escape_map[escape_char]
                    elif escape_char == 'x':
                        # Hex escape \xXX
                        hex_chars = ""
                        for _ in range(2):
                            if self.peek() and self.peek() in '0123456789abcdefABCDEF':
                                hex_chars += self.advance()
                        if hex_chars:
                            value += chr(int(hex_chars, 16))
                    elif escape_char.isdigit():
                        # Decimal escape \ddd
                        num = escape_char
                        for _ in range(2):
                            if self.peek() and self.peek().isdigit():
                                num += self.advance()
                        value += chr(int(num))
                    elif escape_char == 'z':
                        # Skip whitespace
                        while self.peek() and self.peek() in ' \t\n':
                            self.advance()
                    else:
                        value += escape_char
            elif char == '\n':
                self.errors.append(f"Unterminated string at L{start_line}:C{start_col}")
                break
            else:
                value += char
        
        if self.peek() == quote:
            raw += self.advance()
        
        return Token(TokenType.STRING, value, start_line, start_col, raw)
    
    def read_number(self) -> Token:
        """Read numeric literal"""
        start_line, start_col = self.line, self.column
        value = ""
        
        # Hexadecimal
        if self.peek() == '0' and self.peek(1) in 'xX':
            value += self.advance()  # 0
            value += self.advance()  # x/X
            
            while self.peek() and self.peek() in '0123456789abcdefABCDEF':
                value += self.advance()
            
            # Hex float (Lua 5.2+)
            if self.peek() == '.':
                value += self.advance()
                while self.peek() and self.peek() in '0123456789abcdefABCDEF':
                    value += self.advance()
            
            if self.peek() in 'pP':
                value += self.advance()
                if self.peek() in '+-':
                    value += self.advance()
                while self.peek() and self.peek().isdigit():
                    value += self.advance()
            
            try:
                num_val = float.fromhex(value) if '.' in value or 'p' in value.lower() else int(value, 16)
            except:
                num_val = 0
                self.errors.append(f"Invalid hex number at L{start_line}:C{start_col}")
        
        # Binary (Roblox extension)
        elif self.peek() == '0' and self.peek(1) in 'bB':
            value += self.advance()  # 0
            value += self.advance()  # b/B
            
            while self.peek() and self.peek() in '01_':
                if self.peek() != '_':
                    value += self.advance()
                else:
                    self.advance()  # Skip underscore
            
            try:
                num_val = int(value.replace('0b', '').replace('0B', ''), 2)
            except:
                num_val = 0
        
        # Decimal
        else:
            while self.peek() and (self.peek().isdigit() or self.peek() == '_'):
                if self.peek() != '_':
                    value += self.advance()
                else:
                    self.advance()
            
            # Decimal part
            if self.peek() == '.' and self.peek(1) != '.':
                value += self.advance()
                while self.peek() and (self.peek().isdigit() or self.peek() == '_'):
                    if self.peek() != '_':
                        value += self.advance()
                    else:
                        self.advance()
            
            # Exponent
            if self.peek() in 'eE':
                value += self.advance()
                if self.peek() in '+-':
                    value += self.advance()
                while self.peek() and self.peek().isdigit():
                    value += self.advance()
            
            try:
                num_val = float(value) if '.' in value or 'e' in value.lower() else int(value)
            except:
                num_val = 0
                self.errors.append(f"Invalid number at L{start_line}:C{start_col}")
        
        return Token(TokenType.NUMBER, num_val, start_line, start_col, value)
    
    def read_identifier(self) -> Token:
        """Read identifier or keyword"""
        start_line, start_col = self.line, self.column
        value = ""
        
        while self.peek() and (self.peek().isalnum() or self.peek() == '_'):
            value += self.advance()
        
        # Check for keyword
        if value in self.KEYWORDS:
            token_type = self.KEYWORDS[value]
            
            if value == 'true':
                return Token(TokenType.BOOLEAN, True, start_line, start_col, value)
            elif value == 'false':
                return Token(TokenType.BOOLEAN, False, start_line, start_col, value)
            elif value == 'nil':
                return Token(TokenType.NIL, None, start_line, start_col, value)
            
            return Token(token_type, value, start_line, start_col, value)
        
        return Token(TokenType.IDENTIFIER, value, start_line, start_col, value)
    
    def read_operator(self) -> Optional[Token]:
        """Read operator token"""
        start_line, start_col = self.line, self.column
        
        # Three-character operators
        three_char = self.peek_string(3)
        if three_char == '...':
            self.advance()
            self.advance()
            self.advance()
            return Token(TokenType.VARARG, '...', start_line, start_col, '...')
        
        # Two-character operators
        two_char = self.peek_string(2)
        two_char_ops = {
            '==': TokenType.EQ, '~=': TokenType.NEQ, '<=': TokenType.LTE,
            '>=': TokenType.GTE, '..': TokenType.CONCAT, '::': TokenType.DOUBLE_COLON,
            '+=': TokenType.PLUS_EQ, '-=': TokenType.MINUS_EQ,
            '*=': TokenType.STAR_EQ, '/=': TokenType.SLASH_EQ
        }
        
        if two_char in two_char_ops:
            self.advance()
            self.advance()
            return Token(two_char_ops[two_char], two_char, start_line, start_col, two_char)
        
        # Single-character operators
        one_char = self.peek()
        one_char_ops = {
            '+': TokenType.PLUS, '-': TokenType.MINUS, '*': TokenType.STAR,
            '/': TokenType.SLASH, '%': TokenType.PERCENT, '^': TokenType.CARET,
            '#': TokenType.HASH, '<': TokenType.LT, '>': TokenType.GT,
            '=': TokenType.ASSIGN
        }
        
        if one_char in one_char_ops:
            self.advance()
            return Token(one_char_ops[one_char], one_char, start_line, start_col, one_char)
        
        return None
    
    def read_delimiter(self) -> Optional[Token]:
        """Read delimiter token"""
        start_line, start_col = self.line, self.column
        char = self.peek()
        
        delimiters = {
            '(': TokenType.LPAREN, ')': TokenType.RPAREN,
            '{': TokenType.LBRACE, '}': TokenType.RBRACE,
            '[': TokenType.LBRACKET, ']': TokenType.RBRACKET,
            ';': TokenType.SEMICOLON, ':': TokenType.COLON,
            ',': TokenType.COMMA, '.': TokenType.DOT
        }
        
        if char in delimiters:
            self.advance()
            return Token(delimiters[char], char, start_line, start_col, char)
        
        return None
    
    def tokenize(self) -> List[Token]:
        """
        Tokenize entire source code
        
        Returns:
            List of tokens
        """
        self.tokens = []
        self.errors = []
        
        while self.pos < len(self.source):
            self.skip_whitespace()
            
            if self.pos >= len(self.source):
                break
            
            # Skip comments
            if self.skip_comment():
                continue
            
            char = self.peek()
            start_line, start_col = self.line, self.column
            
            # Long string [[...]] or [=[...]=]
            if char == '[' and self.get_long_bracket_level() >= 0:
                self.tokens.append(self.read_long_string())
            
            # Quoted string
            elif char in '"\'':
                self.tokens.append(self.read_string())
            
            # Number
            elif char.isdigit():
                self.tokens.append(self.read_number())
            
            # Dot can start a number (.5) or be operator
            elif char == '.' and self.peek(1) and self.peek(1).isdigit():
                self.tokens.append(self.read_number())
            
            # Identifier or keyword
            elif char.isalpha() or char == '_':
                self.tokens.append(self.read_identifier())
            
            # Operators
            else:
                op_token = self.read_operator()
                if op_token:
                    self.tokens.append(op_token)
                else:
                    delim_token = self.read_delimiter()
                    if delim_token:
                        self.tokens.append(delim_token)
                    else:
                        # Unknown character, skip it
                        self.errors.append(f"Unexpected character '{char}' at L{start_line}:C{start_col}")
                        self.advance()
        
        # Add EOF token
        self.tokens.append(Token(TokenType.EOF, None, self.line, self.column, ''))
        
        return self.tokens
    
    def get_errors(self) -> List[str]:
        """Get list of lexer errors"""
        return self.errors


def tokenize_lua(source: str) -> Tuple[List[Token], List[str]]:
    """
    Convenience function to tokenize Lua source code
    
    Args:
        source: Lua source code as string
        
    Returns:
        Tuple of (tokens, errors)
    """
    lexer = LuaLexer(source)
    tokens = lexer.tokenize()
    return tokens, lexer.get_errors()


def tokenize_file(filepath: str) -> Tuple[List[Token], List[str]]:
    """
    Convenience function to tokenize Lua file
    
    Args:
        filepath: Path to .lua or .txt file
        
    Returns:
        Tuple of (tokens, errors)
    """
    lexer = LuaLexer.from_file(filepath)
    tokens = lexer.tokenize()
    return tokens, lexer.get_errors()
