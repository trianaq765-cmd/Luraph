from dataclasses import dataclass, field
from typing import List, Optional, Any, Union
from .lexer import Token, TokenType, LuaLexer

# ==================== AST NODES ====================
@dataclass
class ASTNode:
    line: int = 0
    column: int = 0

@dataclass
class NumberLiteral(ASTNode):
    value: float = 0

@dataclass
class StringLiteral(ASTNode):
    value: str = ""

@dataclass
class BooleanLiteral(ASTNode):
    value: bool = False

@dataclass
class NilLiteral(ASTNode):
    pass

@dataclass
class VarargLiteral(ASTNode):
    pass

@dataclass
class Identifier(ASTNode):
    name: str = ""

@dataclass
class TableConstructor(ASTNode):
    fields: List[Any] = field(default_factory=list)

@dataclass
class TableField(ASTNode):
    key: Optional[Any] = None
    value: Any = None
    is_array: bool = False

@dataclass
class BinaryOp(ASTNode):
    operator: str = ""
    left: Any = None
    right: Any = None

@dataclass
class UnaryOp(ASTNode):
    operator: str = ""
    operand: Any = None

@dataclass
class FunctionCall(ASTNode):
    callee: Any = None
    args: List[Any] = field(default_factory=list)
    method: Optional[str] = None

@dataclass
class IndexAccess(ASTNode):
    object: Any = None
    index: Any = None

@dataclass
class MemberAccess(ASTNode):
    object: Any = None
    member: str = ""

@dataclass
class MethodAccess(ASTNode):
    """Specifically for method definitions with : syntax"""
    object: Any = None
    method: str = ""

@dataclass
class Assignment(ASTNode):
    targets: List[Any] = field(default_factory=list)
    values: List[Any] = field(default_factory=list)

@dataclass
class CompoundAssignment(ASTNode):
    """Roblox compound assignment +=, -=, etc."""
    target: Any = None
    operator: str = ""
    value: Any = None

@dataclass
class LocalDeclaration(ASTNode):
    names: List[str] = field(default_factory=list)
    values: List[Any] = field(default_factory=list)

@dataclass
class FunctionDecl(ASTNode):
    name: Optional[Any] = None
    params: List[str] = field(default_factory=list)
    body: List[Any] = field(default_factory=list)
    is_local: bool = False
    is_vararg: bool = False
    is_method: bool = False  # If defined with :

@dataclass
class ReturnStmt(ASTNode):
    values: List[Any] = field(default_factory=list)

@dataclass
class BreakStmt(ASTNode):
    pass

@dataclass
class ContinueStmt(ASTNode):
    pass

@dataclass
class IfStmt(ASTNode):
    condition: Any = None
    then_body: List[Any] = field(default_factory=list)
    elseif_clauses: List[Any] = field(default_factory=list)
    else_body: List[Any] = field(default_factory=list)

@dataclass
class ElseIfClause(ASTNode):
    condition: Any = None
    body: List[Any] = field(default_factory=list)

@dataclass
class WhileStmt(ASTNode):
    condition: Any = None
    body: List[Any] = field(default_factory=list)

@dataclass
class RepeatStmt(ASTNode):
    body: List[Any] = field(default_factory=list)
    condition: Any = None

@dataclass
class ForNumeric(ASTNode):
    var: str = ""
    start: Any = None
    stop: Any = None
    step: Optional[Any] = None
    body: List[Any] = field(default_factory=list)

@dataclass
class ForGeneric(ASTNode):
    vars: List[str] = field(default_factory=list)
    iterators: List[Any] = field(default_factory=list)
    body: List[Any] = field(default_factory=list)

@dataclass
class DoBlock(ASTNode):
    body: List[Any] = field(default_factory=list)

@dataclass
class Chunk(ASTNode):
    body: List[Any] = field(default_factory=list)

# ==================== PARSER ====================
class LuaParser:
    """
    Lua Parser - Parses tokens into AST
    Supports: Lua 5.1/5.2/5.3 and Roblox Luau extensions
    """
    
    BINARY_PRIORITY = {
        'or': (1, 1), 'and': (2, 2),
        '<': (3, 3), '>': (3, 3), '<=': (3, 3), '>=': (3, 3), '~=': (3, 3), '==': (3, 3),
        '..': (5, 4),  # Right associative
        '+': (6, 6), '-': (6, 6),
        '*': (7, 7), '/': (7, 7), '%': (7, 7),
        '^': (10, 9)  # Right associative
    }
    
    UNARY_PRIORITY = 8
    
    def __init__(self, tokens: List[Token]):
        self.tokens = [t for t in tokens if t.type != TokenType.EOF]
        self.tokens.append(Token(TokenType.EOF, None, 0, 0))
        self.pos = 0
        self.errors: List[str] = []
        
    @classmethod
    def from_source(cls, source: str) -> 'LuaParser':
        """Create parser directly from Lua source code"""
        lexer = LuaLexer(source)
        tokens = lexer.tokenize()
        return cls(tokens)
    
    @classmethod
    def from_file(cls, filepath: str) -> 'LuaParser':
        """Create parser from file"""
        lexer = LuaLexer.from_file(filepath)
        tokens = lexer.tokenize()
        return cls(tokens)
    
    def current(self) -> Token:
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return self.tokens[-1]
    
    def peek(self, offset: int = 0) -> Token:
        pos = self.pos + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return self.tokens[-1]
    
    def advance(self) -> Token:
        token = self.current()
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
        return token
    
    def match(self, *types: TokenType) -> bool:
        return self.current().type in types
    
    def check_next(self, *types: TokenType) -> bool:
        return self.peek(1).type in types
    
    def expect(self, token_type: TokenType, msg: str = "") -> Token:
        if not self.match(token_type):
            error_msg = f"Expected {token_type.name}, got {self.current().type.name} at L{self.current().line}:C{self.current().column}"
            if msg:
                error_msg += f". {msg}"
            self.errors.append(error_msg)
            raise SyntaxError(error_msg)
        return self.advance()
    
    def parse(self) -> Chunk:
        """Parse source into AST"""
        try:
            body = self.parse_block()
            if not self.match(TokenType.EOF):
                self.errors.append(f"Unexpected token {self.current().type.name} at end")
            return Chunk(body=body, line=1, column=1)
        except SyntaxError as e:
            return Chunk(body=[], line=1, column=1)
    
    def parse_block(self) -> List[ASTNode]:
        """Parse a block of statements"""
        statements = []
        
        end_tokens = (TokenType.EOF, TokenType.END, TokenType.ELSE, 
                      TokenType.ELSEIF, TokenType.UNTIL)
        
        while not self.match(*end_tokens):
            try:
                stmt = self.parse_statement()
                if stmt:
                    if isinstance(stmt, list):
                        statements.extend(stmt)
                    else:
                        statements.append(stmt)
            except SyntaxError:
                # Try to recover by skipping to next statement
                while not self.match(*end_tokens, TokenType.SEMICOLON):
                    self.advance()
                    if self.match(*end_tokens):
                        break
            
            # Optional semicolon
            while self.match(TokenType.SEMICOLON):
                self.advance()
        
        return statements
    
    def parse_statement(self) -> Optional[ASTNode]:
        """Parse a single statement"""
        token = self.current()
        
        if self.match(TokenType.LOCAL):
            return self.parse_local()
        elif self.match(TokenType.FUNCTION):
            return self.parse_function(is_local=False)
        elif self.match(TokenType.IF):
            return self.parse_if()
        elif self.match(TokenType.WHILE):
            return self.parse_while()
        elif self.match(TokenType.FOR):
            return self.parse_for()
        elif self.match(TokenType.REPEAT):
            return self.parse_repeat()
        elif self.match(TokenType.DO):
            return self.parse_do()
        elif self.match(TokenType.RETURN):
            return self.parse_return()
        elif self.match(TokenType.BREAK):
            self.advance()
            return BreakStmt(line=token.line, column=token.column)
        elif self.match(TokenType.CONTINUE):
            self.advance()
            return ContinueStmt(line=token.line, column=token.column)
        elif self.match(TokenType.DOUBLE_COLON):
            # Label ::name::
            return self.parse_label()
        elif self.match(TokenType.GOTO):
            return self.parse_goto()
        else:
            return self.parse_expr_or_assignment()
    
    def parse_local(self) -> ASTNode:
        """Parse local declaration or local function"""
        token = self.expect(TokenType.LOCAL)
        
        if self.match(TokenType.FUNCTION):
            return self.parse_function(is_local=True)
        
        # Local variable declaration
        names = [self.expect(TokenType.IDENTIFIER).value]
        
        while self.match(TokenType.COMMA):
            self.advance()
            names.append(self.expect(TokenType.IDENTIFIER).value)
        
        values = []
        if self.match(TokenType.ASSIGN):
            self.advance()
            values = self.parse_expr_list()
        
        return LocalDeclaration(names=names, values=values, 
                               line=token.line, column=token.column)
    
    def parse_function(self, is_local: bool = False) -> FunctionDecl:
        """Parse function declaration"""
        token = self.expect(TokenType.FUNCTION)
        
        name = None
        is_method = False
        
        if is_local:
            # local function name() 
            if self.match(TokenType.IDENTIFIER):
                name = Identifier(name=self.advance().value,
                                 line=self.current().line, column=self.current().column)
        else:
            # function name() or function obj.name() or function obj:name()
            name, is_method = self.parse_function_name()
        
        params, is_vararg = self.parse_function_params()
        
        # For methods, add implicit 'self' parameter
        if is_method and 'self' not in params:
            params = ['self'] + params
        
        body = self.parse_block()
        self.expect(TokenType.END, "to close function")
        
        return FunctionDecl(name=name, params=params, body=body,
                           is_local=is_local, is_vararg=is_vararg,
                           is_method=is_method,
                           line=token.line, column=token.column)
    
    def parse_function_name(self) -> tuple:
        """Parse function name (can include . and :)"""
        if not self.match(TokenType.IDENTIFIER):
            return None, False
        
        name = Identifier(name=self.advance().value,
                         line=self.current().line, column=self.current().column)
        is_method = False
        
        # Handle obj.member.member...
        while self.match(TokenType.DOT):
            self.advance()
            member = self.expect(TokenType.IDENTIFIER).value
            name = MemberAccess(object=name, member=member,
                               line=self.current().line, column=self.current().column)
        
        # Handle obj:method (method definition)
        if self.match(TokenType.COLON):
            self.advance()
            method = self.expect(TokenType.IDENTIFIER).value
            name = MethodAccess(object=name, method=method,
                               line=self.current().line, column=self.current().column)
            is_method = True
        
        return name, is_method
    
    def parse_function_params(self) -> tuple:
        """Parse function parameters"""
        self.expect(TokenType.LPAREN)
        params = []
        is_vararg = False
        
        if not self.match(TokenType.RPAREN):
            while True:
                if self.match(TokenType.VARARG):
                    self.advance()
                    is_vararg = True
                    break
                
                if self.match(TokenType.IDENTIFIER):
                    params.append(self.advance().value)
                
                if not self.match(TokenType.COMMA):
                    break
                self.advance()
        
        self.expect(TokenType.RPAREN)
        return params, is_vararg
    
    def parse_if(self) -> IfStmt:
        """Parse if statement"""
        token = self.expect(TokenType.IF)
        condition = self.parse_expression()
        self.expect(TokenType.THEN, "after if condition")
        then_body = self.parse_block()
        
        elseif_clauses = []
        while self.match(TokenType.ELSEIF):
            elseif_token = self.advance()
            elseif_cond = self.parse_expression()
            self.expect(TokenType.THEN, "after elseif condition")
            elseif_body = self.parse_block()
            elseif_clauses.append(ElseIfClause(
                condition=elseif_cond, body=elseif_body,
                line=elseif_token.line, column=elseif_token.column
            ))
        
        else_body = []
        if self.match(TokenType.ELSE):
            self.advance()
            else_body = self.parse_block()
        
        self.expect(TokenType.END, "to close if statement")
        
        return IfStmt(condition=condition, then_body=then_body,
                     elseif_clauses=elseif_clauses, else_body=else_body,
                     line=token.line, column=token.column)
    
    def parse_while(self) -> WhileStmt:
        """Parse while loop"""
        token = self.expect(TokenType.WHILE)
        condition = self.parse_expression()
        self.expect(TokenType.DO, "after while condition")
        body = self.parse_block()
        self.expect(TokenType.END, "to close while loop")
        
        return WhileStmt(condition=condition, body=body,
                        line=token.line, column=token.column)
    
    def parse_for(self) -> ASTNode:
        """Parse for loop (numeric or generic)"""
        token = self.expect(TokenType.FOR)
        first_var = self.expect(TokenType.IDENTIFIER).value
        
        # Numeric for: for i = start, stop, step do
        if self.match(TokenType.ASSIGN):
            self.advance()
            start = self.parse_expression()
            self.expect(TokenType.COMMA)
            stop = self.parse_expression()
            
            step = None
            if self.match(TokenType.COMMA):
                self.advance()
                step = self.parse_expression()
            
            self.expect(TokenType.DO, "after for loop header")
            body = self.parse_block()
            self.expect(TokenType.END, "to close for loop")
            
            return ForNumeric(var=first_var, start=start, stop=stop, step=step,
                             body=body, line=token.line, column=token.column)
        
        # Generic for: for k, v in pairs(t) do
        else:
            vars = [first_var]
            while self.match(TokenType.COMMA):
                self.advance()
                vars.append(self.expect(TokenType.IDENTIFIER).value)
            
            self.expect(TokenType.IN)
            iterators = self.parse_expr_list()
            self.expect(TokenType.DO, "after for loop header")
            body = self.parse_block()
            self.expect(TokenType.END, "to close for loop")
            
            return ForGeneric(vars=vars, iterators=iterators, body=body,
                             line=token.line, column=token.column)
    
    def parse_repeat(self) -> RepeatStmt:
        """Parse repeat...until loop"""
        token = self.expect(TokenType.REPEAT)
        body = self.parse_block()
        self.expect(TokenType.UNTIL, "to close repeat loop")
        condition = self.parse_expression()
        
        return RepeatStmt(body=body, condition=condition,
                         line=token.line, column=token.column)
    
    def parse_do(self) -> DoBlock:
        """Parse do...end block"""
        token = self.expect(TokenType.DO)
        body = self.parse_block()
        self.expect(TokenType.END, "to close do block")
        
        return DoBlock(body=body, line=token.line, column=token.column)
    
    def parse_return(self) -> ReturnStmt:
        """Parse return statement"""
        token = self.expect(TokenType.RETURN)
        
        values = []
        if not self.match(TokenType.END, TokenType.ELSE, TokenType.ELSEIF,
                         TokenType.UNTIL, TokenType.EOF, TokenType.SEMICOLON):
            values = self.parse_expr_list()
        
        return ReturnStmt(values=values, line=token.line, column=token.column)
    
    def parse_label(self) -> ASTNode:
        """Parse ::label::"""
        self.expect(TokenType.DOUBLE_COLON)
        name = self.expect(TokenType.IDENTIFIER).value
        self.expect(TokenType.DOUBLE_COLON)
        # Labels are ignored in VM for now
        return None
    
    def parse_goto(self) -> ASTNode:
        """Parse goto statement"""
        self.expect(TokenType.GOTO)
        self.expect(TokenType.IDENTIFIER)
        # Goto is ignored in VM for now
        return None
    
    def parse_expr_or_assignment(self) -> ASTNode:
        """Parse expression or assignment statement"""
        exprs = [self.parse_prefix_expr()]
        
        while self.match(TokenType.COMMA):
            self.advance()
            exprs.append(self.parse_prefix_expr())
        
        # Compound assignment (Roblox): a += b
        if self.match(TokenType.PLUS_EQ, TokenType.MINUS_EQ, 
                     TokenType.STAR_EQ, TokenType.SLASH_EQ):
            if len(exprs) != 1:
                self.errors.append("Compound assignment requires single target")
            op_token = self.advance()
            op_map = {
                TokenType.PLUS_EQ: '+', TokenType.MINUS_EQ: '-',
                TokenType.STAR_EQ: '*', TokenType.SLASH_EQ: '/'
            }
            value = self.parse_expression()
            return CompoundAssignment(target=exprs[0], operator=op_map[op_token.type],
                                      value=value, line=exprs[0].line, column=exprs[0].column)
        
        # Regular assignment
        if self.match(TokenType.ASSIGN):
            self.advance()
            values = self.parse_expr_list()
            return Assignment(targets=exprs, values=values,
                            line=exprs[0].line, column=exprs[0].column)
        
        # Expression statement (function call)
        if len(exprs) == 1:
            return exprs[0]
        
        return exprs[0]
    
    def parse_expr_list(self) -> List[ASTNode]:
        """Parse comma-separated expression list"""
        exprs = [self.parse_expression()]
        while self.match(TokenType.COMMA):
            self.advance()
            exprs.append(self.parse_expression())
        return exprs
    
    def parse_expression(self, min_priority: int = 0) -> ASTNode:
        """Parse expression with operator precedence"""
        left = self.parse_unary()
        
        while True:
            op = self.current().value
            if op not in self.BINARY_PRIORITY:
                break
            
            left_priority, right_priority = self.BINARY_PRIORITY[op]
            if left_priority < min_priority:
                break
            
            self.advance()
            right = self.parse_expression(right_priority)
            left = BinaryOp(operator=op, left=left, right=right,
                           line=left.line, column=left.column)
        
        return left
    
    def parse_unary(self) -> ASTNode:
        """Parse unary expression"""
        if self.match(TokenType.NOT):
            token = self.advance()
            operand = self.parse_unary()
            return UnaryOp(operator='not', operand=operand,
                          line=token.line, column=token.column)
        elif self.match(TokenType.MINUS):
            token = self.advance()
            operand = self.parse_unary()
            return UnaryOp(operator='-', operand=operand,
                          line=token.line, column=token.column)
        elif self.match(TokenType.HASH):
            token = self.advance()
            operand = self.parse_unary()
            return UnaryOp(operator='#', operand=operand,
                          line=token.line, column=token.column)
        
        return self.parse_power()
    
    def parse_power(self) -> ASTNode:
        """Parse power expression (right associative)"""
        base = self.parse_prefix_expr()
        
        if self.match(TokenType.CARET):
            self.advance()
            exp = self.parse_unary()  # Right associative
            return BinaryOp(operator='^', left=base, right=exp,
                           line=base.line, column=base.column)
        
        return base
    
    def parse_prefix_expr(self) -> ASTNode:
        """Parse prefix expression (calls, indexing, etc.)"""
        expr = self.parse_primary()
        
        while True:
            if self.match(TokenType.DOT):
                self.advance()
                member = self.expect(TokenType.IDENTIFIER).value
                expr = MemberAccess(object=expr, member=member,
                                   line=expr.line, column=expr.column)
            
            elif self.match(TokenType.LBRACKET):
                self.advance()
                index = self.parse_expression()
                self.expect(TokenType.RBRACKET)
                expr = IndexAccess(object=expr, index=index,
                                  line=expr.line, column=expr.column)
            
            elif self.match(TokenType.COLON):
                self.advance()
                method = self.expect(TokenType.IDENTIFIER).value
                args = self.parse_call_args()
                expr = FunctionCall(callee=expr, args=args, method=method,
                                   line=expr.line, column=expr.column)
            
            elif self.match(TokenType.LPAREN, TokenType.LBRACE, TokenType.STRING):
                args = self.parse_call_args()
                expr = FunctionCall(callee=expr, args=args,
                                   line=expr.line, column=expr.column)
            else:
                break
        
        return expr
    
    def parse_call_args(self) -> List[ASTNode]:
        """Parse function call arguments"""
        if self.match(TokenType.LPAREN):
            self.advance()
            args = []
            if not self.match(TokenType.RPAREN):
                args = self.parse_expr_list()
            self.expect(TokenType.RPAREN)
            return args
        
        elif self.match(TokenType.LBRACE):
            return [self.parse_table()]
        
        elif self.match(TokenType.STRING):
            token = self.advance()
            return [StringLiteral(value=token.value, line=token.line, column=token.column)]
        
        return []
    
    def parse_primary(self) -> ASTNode:
        """Parse primary expression"""
        token = self.current()
        
        if self.match(TokenType.NUMBER):
            self.advance()
            return NumberLiteral(value=token.value, line=token.line, column=token.column)
        
        elif self.match(TokenType.STRING):
            self.advance()
            return StringLiteral(value=token.value, line=token.line, column=token.column)
        
        elif self.match(TokenType.BOOLEAN):
            self.advance()
            return BooleanLiteral(value=token.value, line=token.line, column=token.column)
        
        elif self.match(TokenType.NIL):
            self.advance()
            return NilLiteral(line=token.line, column=token.column)
        
        elif self.match(TokenType.VARARG):
            self.advance()
            return VarargLiteral(line=token.line, column=token.column)
        
        elif self.match(TokenType.IDENTIFIER):
            self.advance()
            return Identifier(name=token.value, line=token.line, column=token.column)
        
        elif self.match(TokenType.LPAREN):
            self.advance()
            expr = self.parse_expression()
            self.expect(TokenType.RPAREN)
            return expr
        
        elif self.match(TokenType.LBRACE):
            return self.parse_table()
        
        elif self.match(TokenType.FUNCTION):
            return self.parse_anonymous_function()
        
        self.errors.append(f"Unexpected token {token.type.name} at L{token.line}:C{token.column}")
        raise SyntaxError(f"Unexpected token {token.type.name}")
    
    def parse_table(self) -> TableConstructor:
        """Parse table constructor"""
        token = self.expect(TokenType.LBRACE)
        fields = []
        
        while not self.match(TokenType.RBRACE):
            # [expr] = value
            if self.match(TokenType.LBRACKET):
                self.advance()
                key = self.parse_expression()
                self.expect(TokenType.RBRACKET)
                self.expect(TokenType.ASSIGN)
                value = self.parse_expression()
                fields.append(TableField(key=key, value=value, is_array=False,
                                        line=key.line, column=key.column))
            
            # name = value or array value
            elif self.match(TokenType.IDENTIFIER) and self.check_next(TokenType.ASSIGN):
                key_token = self.advance()
                self.advance()  # =
                value = self.parse_expression()
                fields.append(TableField(
                    key=StringLiteral(value=key_token.value, line=key_token.line, column=key_token.column),
                    value=value, is_array=False,
                    line=key_token.line, column=key_token.column
                ))
            
            # Array value
            else:
                value = self.parse_expression()
                fields.append(TableField(key=None, value=value, is_array=True,
                                        line=value.line, column=value.column))
            
            # Separator
            if self.match(TokenType.COMMA, TokenType.SEMICOLON):
                self.advance()
            elif not self.match(TokenType.RBRACE):
                break
        
        self.expect(TokenType.RBRACE)
        return TableConstructor(fields=fields, line=token.line, column=token.column)
    
    def parse_anonymous_function(self) -> FunctionDecl:
        """Parse anonymous function"""
        token = self.expect(TokenType.FUNCTION)
        params, is_vararg = self.parse_function_params()
        body = self.parse_block()
        self.expect(TokenType.END)
        
        return FunctionDecl(name=None, params=params, body=body,
                           is_local=False, is_vararg=is_vararg,
                           line=token.line, column=token.column)
    
    def get_errors(self) -> List[str]:
        """Get parser errors"""
        return self.errors


def parse_lua(source: str) -> tuple:
    """
    Convenience function to parse Lua source code
    
    Args:
        source: Lua source code string
        
    Returns:
        Tuple of (AST, errors)
    """
    parser = LuaParser.from_source(source)
    ast = parser.parse()
    return ast, parser.get_errors()


def parse_file(filepath: str) -> tuple:
    """
    Convenience function to parse Lua file
    
    Args:
        filepath: Path to .lua or .txt file
        
    Returns:
        Tuple of (AST, errors)
    """
    parser = LuaParser.from_file(filepath)
    ast = parser.parse()
    return ast, parser.get_errors()
