from enum import IntEnum
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass, field
from .parser import (
    ASTNode, NumberLiteral, StringLiteral, BooleanLiteral, NilLiteral,
    VarargLiteral, Identifier, TableConstructor, TableField, BinaryOp,
    UnaryOp, FunctionCall, IndexAccess, MemberAccess, MethodAccess,
    Assignment, CompoundAssignment, LocalDeclaration, FunctionDecl,
    ReturnStmt, BreakStmt, ContinueStmt, IfStmt, ElseIfClause,
    WhileStmt, RepeatStmt, ForNumeric, ForGeneric, DoBlock, Chunk
)

class OpCode(IntEnum):
    """
    Virtual Machine OpCodes
    Designed for stack-based VM execution
    """
    # Stack Operations
    LOAD_CONST = 0x01
    LOAD_NIL = 0x02
    LOAD_TRUE = 0x03
    LOAD_FALSE = 0x04
    LOAD_VAR = 0x05
    STORE_VAR = 0x06
    LOAD_GLOBAL = 0x07
    STORE_GLOBAL = 0x08
    LOAD_UPVAL = 0x09
    STORE_UPVAL = 0x0A
    
    # Table Operations
    NEW_TABLE = 0x10
    GET_TABLE = 0x11
    SET_TABLE = 0x12
    GET_FIELD = 0x13
    SET_FIELD = 0x14
    APPEND_TABLE = 0x15
    
    # Arithmetic
    ADD = 0x20
    SUB = 0x21
    MUL = 0x22
    DIV = 0x23
    MOD = 0x24
    POW = 0x25
    UNM = 0x26
    CONCAT = 0x27
    LEN = 0x28
    
    # Comparison
    EQ = 0x30
    NE = 0x31
    LT = 0x32
    LE = 0x33
    GT = 0x34
    GE = 0x35
    
    # Logical
    NOT = 0x40
    AND = 0x41
    OR = 0x42
    TEST = 0x43
    
    # Control Flow
    JMP = 0x50
    JMP_IF = 0x51
    JMP_IF_NOT = 0x52
    JMP_IF_NIL = 0x53
    JMP_IF_FALSE = 0x54
    JMP_BACK = 0x55
    
    # Function
    CALL = 0x60
    RETURN = 0x61
    CLOSURE = 0x62
    VARARG = 0x63
    SELF = 0x64
    TAILCALL = 0x65
    
    # For Loop
    FORPREP = 0x70
    FORLOOP = 0x71
    TFORLOOP = 0x72
    TFORCALL = 0x73
    
    # Misc
    POP = 0x80
    DUP = 0x81
    DUP2 = 0x82
    SWAP = 0x83
    MOVE = 0x84
    CLOSE = 0x85
    NOP = 0xFF


@dataclass
class Instruction:
    """Single VM instruction"""
    opcode: OpCode
    operand: Any = None
    operand2: Any = None  # For instructions with 2 operands
    line: int = 0
    
    def __repr__(self):
        if self.operand2 is not None:
            return f"{self.opcode.name}({self.operand}, {self.operand2})"
        elif self.operand is not None:
            return f"{self.opcode.name}({self.operand})"
        return self.opcode.name


@dataclass
class CompiledFunction:
    """Compiled function bytecode container"""
    name: str = ""
    params: List[str] = field(default_factory=list)
    is_vararg: bool = False
    is_method: bool = False
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[Any] = field(default_factory=list)
    locals: List[str] = field(default_factory=list)
    upvalues: List[Tuple[str, int, bool]] = field(default_factory=list)  # (name, index, is_local)
    children: List['CompiledFunction'] = field(default_factory=list)
    max_stack: int = 256
    num_params: int = 0
    
    def __post_init__(self):
        self.num_params = len(self.params)


@dataclass
class LoopContext:
    """Context for loop compilation (break/continue support)"""
    start_pc: int
    break_jumps: List[int] = field(default_factory=list)
    continue_jumps: List[int] = field(default_factory=list)


@dataclass
class Scope:
    """Variable scope"""
    locals: Dict[str, int] = field(default_factory=dict)
    start_local: int = 0


@dataclass
class CompilerContext:
    """Compiler context for a single function"""
    function: CompiledFunction = field(default_factory=CompiledFunction)
    parent: Optional['CompilerContext'] = None
    scopes: List[Scope] = field(default_factory=list)
    loops: List[LoopContext] = field(default_factory=list)
    
    def __post_init__(self):
        if not self.scopes:
            self.scopes = [Scope()]
    
    def enter_scope(self):
        """Enter a new variable scope"""
        start = len(self.function.locals)
        self.scopes.append(Scope(start_local=start))
    
    def leave_scope(self):
        """Leave current variable scope"""
        if len(self.scopes) > 1:
            self.scopes.pop()
    
    def current_scope(self) -> Scope:
        return self.scopes[-1]
    
    def add_local(self, name: str) -> int:
        """Add a local variable"""
        idx = len(self.function.locals)
        self.function.locals.append(name)
        self.current_scope().locals[name] = idx
        return idx
    
    def find_local(self, name: str) -> int:
        """Find local variable index, -1 if not found"""
        for scope in reversed(self.scopes):
            if name in scope.locals:
                return scope.locals[name]
        return -1
    
    def find_upvalue(self, name: str) -> int:
        """Find or create upvalue, -1 if not found"""
        # Check if already registered
        for i, (uv_name, _, _) in enumerate(self.function.upvalues):
            if uv_name == name:
                return i
        
        if not self.parent:
            return -1
        
        # Check parent's locals
        local_idx = self.parent.find_local(name)
        if local_idx >= 0:
            idx = len(self.function.upvalues)
            self.function.upvalues.append((name, local_idx, True))
            return idx
        
        # Check parent's upvalues
        parent_upval = self.parent.find_upvalue(name)
        if parent_upval >= 0:
            idx = len(self.function.upvalues)
            self.function.upvalues.append((name, parent_upval, False))
            return idx
        
        return -1
    
    def enter_loop(self, start_pc: int):
        """Enter a loop"""
        self.loops.append(LoopContext(start_pc=start_pc))
    
    def leave_loop(self) -> LoopContext:
        """Leave current loop"""
        return self.loops.pop() if self.loops else None
    
    def current_loop(self) -> Optional[LoopContext]:
        """Get current loop context"""
        return self.loops[-1] if self.loops else None


class BytecodeCompiler:
    """
    Lua Bytecode Compiler
    Compiles AST to bytecode for VM execution
    """
    
    def __init__(self):
        self.ctx: Optional[CompilerContext] = None
        self.errors: List[str] = []
        
    def compile(self, ast: Chunk) -> Tuple[CompiledFunction, List[str]]:
        """
        Compile AST to bytecode
        
        Args:
            ast: Parsed Lua AST
            
        Returns:
            Tuple of (CompiledFunction, errors)
        """
        self.errors = []
        self.ctx = CompilerContext()
        self.ctx.function.name = "__main__"
        
        try:
            for stmt in ast.body:
                self.compile_statement(stmt)
            
            # Implicit return nil at end
            self.emit(OpCode.LOAD_NIL)
            self.emit(OpCode.RETURN, 1)
        except Exception as e:
            self.errors.append(f"Compilation error: {str(e)}")
        
        return self.ctx.function, self.errors
    
    def emit(self, opcode: OpCode, operand: Any = None, 
             operand2: Any = None, line: int = 0) -> int:
        """Emit an instruction"""
        idx = len(self.ctx.function.instructions)
        instr = Instruction(opcode, operand, operand2, line)
        self.ctx.function.instructions.append(instr)
        return idx
    
    def current_pc(self) -> int:
        """Get current program counter"""
        return len(self.ctx.function.instructions)
    
    def patch_jump(self, idx: int, target: int = None):
        """Patch a jump instruction with target"""
        if target is None:
            target = self.current_pc()
        self.ctx.function.instructions[idx].operand = target
    
    def add_constant(self, value: Any) -> int:
        """Add a constant to the constant pool"""
        # Check if constant already exists
        for i, const in enumerate(self.ctx.function.constants):
            if const == value and type(const) == type(value):
                return i
        
        idx = len(self.ctx.function.constants)
        self.ctx.function.constants.append(value)
        return idx
    
    def compile_statement(self, node: ASTNode):
        """Compile a statement"""
        if node is None:
            return
        
        line = getattr(node, 'line', 0)
        
        if isinstance(node, LocalDeclaration):
            self.compile_local_declaration(node)
        elif isinstance(node, Assignment):
            self.compile_assignment(node)
        elif isinstance(node, CompoundAssignment):
            self.compile_compound_assignment(node)
        elif isinstance(node, FunctionDecl):
            self.compile_function_declaration(node)
        elif isinstance(node, FunctionCall):
            self.compile_expression(node)
            self.emit(OpCode.POP, line=line)
        elif isinstance(node, IfStmt):
            self.compile_if(node)
        elif isinstance(node, WhileStmt):
            self.compile_while(node)
        elif isinstance(node, ForNumeric):
            self.compile_for_numeric(node)
        elif isinstance(node, ForGeneric):
            self.compile_for_generic(node)
        elif isinstance(node, RepeatStmt):
            self.compile_repeat(node)
        elif isinstance(node, DoBlock):
            self.compile_do_block(node)
        elif isinstance(node, ReturnStmt):
            self.compile_return(node)
        elif isinstance(node, BreakStmt):
            self.compile_break(node)
        elif isinstance(node, ContinueStmt):
            self.compile_continue(node)
        else:
            # Expression statement
            self.compile_expression(node)
            self.emit(OpCode.POP, line=line)
    
    def compile_local_declaration(self, node: LocalDeclaration):
        """Compile local variable declaration"""
        line = node.line
        
        # Compile values first
        num_names = len(node.names)
        num_values = len(node.values)
        
        for i, value in enumerate(node.values):
            self.compile_expression(value)
        
        # Pad with nil if needed
        for i in range(num_values, num_names):
            self.emit(OpCode.LOAD_NIL, line=line)
        
        # Pop extra values if more values than names
        for i in range(num_names, num_values):
            self.emit(OpCode.POP, line=line)
        
        # Store to locals (in reverse order since values are on stack)
        for name in reversed(node.names):
            local_idx = self.ctx.add_local(name)
            self.emit(OpCode.STORE_VAR, local_idx, line=line)
    
    def compile_assignment(self, node: Assignment):
        """Compile assignment statement"""
        line = node.line
        num_targets = len(node.targets)
        num_values = len(node.values)
        
        # Compile all values
        for value in node.values:
            self.compile_expression(value)
        
        # Pad with nil if needed
        for i in range(num_values, num_targets):
            self.emit(OpCode.LOAD_NIL, line=line)
        
        # Store to targets in reverse order
        for target in reversed(node.targets):
            self.compile_store_target(target)
    
    def compile_compound_assignment(self, node: CompoundAssignment):
        """Compile compound assignment (+=, -=, etc.)"""
        line = node.line
        
        # Load current value
        self.compile_expression(node.target)
        
        # Compile right side value
        self.compile_expression(node.value)
        
        # Apply operator
        op_map = {
            '+': OpCode.ADD,
            '-': OpCode.SUB,
            '*': OpCode.MUL,
            '/': OpCode.DIV
        }
        self.emit(op_map[node.operator], line=line)
        
        # Store result
        self.compile_store_target(node.target)
    
    def compile_store_target(self, target: ASTNode):
        """Compile storing to a target (variable, table index, etc.)"""
        line = getattr(target, 'line', 0)
        
        if isinstance(target, Identifier):
            local_idx = self.ctx.find_local(target.name)
            if local_idx >= 0:
                self.emit(OpCode.STORE_VAR, local_idx, line=line)
            else:
                upval_idx = self.ctx.find_upvalue(target.name)
                if upval_idx >= 0:
                    self.emit(OpCode.STORE_UPVAL, upval_idx, line=line)
                else:
                    const_idx = self.add_constant(target.name)
                    self.emit(OpCode.STORE_GLOBAL, const_idx, line=line)
        
        elif isinstance(target, IndexAccess):
            self.compile_expression(target.object)
            self.compile_expression(target.index)
            self.emit(OpCode.SET_TABLE, line=line)
        
        elif isinstance(target, MemberAccess):
            self.compile_expression(target.object)
            const_idx = self.add_constant(target.member)
            self.emit(OpCode.SET_FIELD, const_idx, line=line)
    
    def compile_function_declaration(self, node: FunctionDecl):
        """Compile function declaration"""
        line = node.line
        
        # Compile the function body
        func = self.compile_function_body(node)
        self.ctx.function.children.append(func)
        child_idx = len(self.ctx.function.children) - 1
        
        # Create closure
        self.emit(OpCode.CLOSURE, child_idx, line=line)
        
        # Store function
        if node.name:
            if node.is_local:
                # local function name()
                if isinstance(node.name, Identifier):
                    local_idx = self.ctx.add_local(node.name.name)
                    self.emit(OpCode.STORE_VAR, local_idx, line=line)
            else:
                # function name() or function obj.name() or function obj:method()
                self.compile_store_function_name(node.name)
        else:
            # Anonymous function - leave on stack
            pass
    
    def compile_store_function_name(self, name: ASTNode):
        """Compile storing function to its name location"""
        line = getattr(name, 'line', 0)
        
        if isinstance(name, Identifier):
            const_idx = self.add_constant(name.name)
            self.emit(OpCode.STORE_GLOBAL, const_idx, line=line)
        
        elif isinstance(name, MemberAccess):
            self.compile_expression(name.object)
            const_idx = self.add_constant(name.member)
            self.emit(OpCode.SET_FIELD, const_idx, line=line)
        
        elif isinstance(name, MethodAccess):
            self.compile_expression(name.object)
            const_idx = self.add_constant(name.method)
            self.emit(OpCode.SET_FIELD, const_idx, line=line)
    
    def compile_function_body(self, node: FunctionDecl) -> CompiledFunction:
        """Compile function body to a new CompiledFunction"""
        parent_ctx = self.ctx
        self.ctx = CompilerContext(parent=parent_ctx)
        
        # Set function name
        if isinstance(node.name, Identifier):
            self.ctx.function.name = node.name.name
        elif isinstance(node.name, MemberAccess):
            self.ctx.function.name = node.name.member
        elif isinstance(node.name, MethodAccess):
            self.ctx.function.name = node.name.method
        else:
            self.ctx.function.name = "__anon__"
        
        self.ctx.function.is_vararg = node.is_vararg
        self.ctx.function.is_method = node.is_method
        self.ctx.function.params = node.params.copy()
        
        # Register parameters as locals
        for param in node.params:
            self.ctx.add_local(param)
        
        # Compile body
        for stmt in node.body:
            self.compile_statement(stmt)
        
        # Implicit return nil
        self.emit(OpCode.LOAD_NIL)
        self.emit(OpCode.RETURN, 1)
        
        func = self.ctx.function
        self.ctx = parent_ctx
        return func
    
    def compile_if(self, node: IfStmt):
        """Compile if statement"""
        line = node.line
        end_jumps = []
        
        # Compile condition
        self.compile_expression(node.condition)
        else_jump = self.emit(OpCode.JMP_IF_NOT, 0, line=line)
        
        # Compile then body
        self.ctx.enter_scope()
        for stmt in node.then_body:
            self.compile_statement(stmt)
        self.ctx.leave_scope()
        
        # Jump to end after then
        if node.elseif_clauses or node.else_body:
            end_jumps.append(self.emit(OpCode.JMP, 0))
        
        self.patch_jump(else_jump)
        
        # Compile elseif clauses
        for clause in node.elseif_clauses:
            self.compile_expression(clause.condition)
            next_jump = self.emit(OpCode.JMP_IF_NOT, 0)
            
            self.ctx.enter_scope()
            for stmt in clause.body:
                self.compile_statement(stmt)
            self.ctx.leave_scope()
            
            end_jumps.append(self.emit(OpCode.JMP, 0))
            self.patch_jump(next_jump)
        
        # Compile else body
        if node.else_body:
            self.ctx.enter_scope()
            for stmt in node.else_body:
                self.compile_statement(stmt)
            self.ctx.leave_scope()
        
        # Patch all end jumps
        for jump in end_jumps:
            self.patch_jump(jump)
    
    def compile_while(self, node: WhileStmt):
        """Compile while loop"""
        line = node.line
        
        loop_start = self.current_pc()
        self.ctx.enter_loop(loop_start)
        self.ctx.enter_scope()
        
        # Compile condition
        self.compile_expression(node.condition)
        exit_jump = self.emit(OpCode.JMP_IF_NOT, 0, line=line)
        
        # Compile body
        for stmt in node.body:
            self.compile_statement(stmt)
        
        # Jump back to start
        self.emit(OpCode.JMP, loop_start, line=line)
        self.patch_jump(exit_jump)
        
        # Patch break/continue
        loop_ctx = self.ctx.leave_loop()
        for break_jump in loop_ctx.break_jumps:
            self.patch_jump(break_jump)
        for continue_jump in loop_ctx.continue_jumps:
            self.patch_jump(continue_jump, loop_start)
        
        self.ctx.leave_scope()
    
    def compile_for_numeric(self, node: ForNumeric):
        """Compile numeric for loop"""
        line = node.line
        
        self.ctx.enter_scope()
        
        # Compile loop control expressions
        self.compile_expression(node.start)
        self.compile_expression(node.stop)
        if node.step:
            self.compile_expression(node.step)
        else:
            self.emit(OpCode.LOAD_CONST, self.add_constant(1), line=line)
        
        # Create loop variable
        var_idx = self.ctx.add_local(node.var)
        
        # FORPREP: initialize loop
        prep_jump = self.emit(OpCode.FORPREP, var_idx, line=line)
        
        loop_start = self.current_pc()
        self.ctx.enter_loop(loop_start)
        
        # Compile body
        for stmt in node.body:
            self.compile_statement(stmt)
        
        # Continue point
        continue_target = self.current_pc()
        
        # FORLOOP: increment and test
        self.emit(OpCode.FORLOOP, loop_start, line=line)
        
        # Patch prep jump
        self.patch_jump(prep_jump)
        
        # Patch break/continue
        loop_ctx = self.ctx.leave_loop()
        for break_jump in loop_ctx.break_jumps:
            self.patch_jump(break_jump)
        for continue_jump in loop_ctx.continue_jumps:
            self.patch_jump(continue_jump, continue_target)
        
        self.ctx.leave_scope()
    
    def compile_for_generic(self, node: ForGeneric):
        """Compile generic for loop"""
        line = node.line
        
        self.ctx.enter_scope()
        
        # Compile iterators
        for iterator in node.iterators:
            self.compile_expression(iterator)
        
        # Pad to 3 values if needed (iterator, state, initial)
        num_iters = len(node.iterators)
        for _ in range(num_iters, 3):
            self.emit(OpCode.LOAD_NIL, line=line)
        
        # Create loop variables
        for var in node.vars:
            self.ctx.add_local(var)
        
        loop_start = self.current_pc()
        self.ctx.enter_loop(loop_start)
        
        # TFORLOOP: call iterator and test
        exit_jump = self.emit(OpCode.TFORLOOP, len(node.vars), line=line)
        
        # Compile body
        for stmt in node.body:
            self.compile_statement(stmt)
        
        # Jump back
        self.emit(OpCode.JMP, loop_start, line=line)
        
        self.patch_jump(exit_jump)
        
        # Patch break/continue
        loop_ctx = self.ctx.leave_loop()
        for break_jump in loop_ctx.break_jumps:
            self.patch_jump(break_jump)
        for continue_jump in loop_ctx.continue_jumps:
            self.patch_jump(continue_jump, loop_start)
        
        self.ctx.leave_scope()
    
    def compile_repeat(self, node: RepeatStmt):
        """Compile repeat...until loop"""
        line = node.line
        
        loop_start = self.current_pc()
        self.ctx.enter_loop(loop_start)
        self.ctx.enter_scope()
        
        # Compile body
        for stmt in node.body:
            self.compile_statement(stmt)
        
        # Continue point (before condition)
        continue_target = self.current_pc()
        
        # Compile condition
        self.compile_expression(node.condition)
        self.emit(OpCode.JMP_IF_NOT, loop_start, line=line)
        
        # Patch break/continue
        loop_ctx = self.ctx.leave_loop()
        for break_jump in loop_ctx.break_jumps:
            self.patch_jump(break_jump)
        for continue_jump in loop_ctx.continue_jumps:
            self.patch_jump(continue_jump, continue_target)
        
        self.ctx.leave_scope()
    
    def compile_do_block(self, node: DoBlock):
        """Compile do...end block"""
        self.ctx.enter_scope()
        for stmt in node.body:
            self.compile_statement(stmt)
        self.ctx.leave_scope()
    
    def compile_return(self, node: ReturnStmt):
        """Compile return statement"""
        line = node.line
        
        for value in node.values:
            self.compile_expression(value)
        
        self.emit(OpCode.RETURN, len(node.values), line=line)
    
    def compile_break(self, node: BreakStmt):
        """Compile break statement"""
        loop = self.ctx.current_loop()
        if loop:
            jump = self.emit(OpCode.JMP, 0, line=node.line)
            loop.break_jumps.append(jump)
        else:
            self.errors.append(f"'break' outside loop at L{node.line}")
    
    def compile_continue(self, node: ContinueStmt):
        """Compile continue statement (Roblox extension)"""
        loop = self.ctx.current_loop()
        if loop:
            jump = self.emit(OpCode.JMP, 0, line=node.line)
            loop.continue_jumps.append(jump)
        else:
            self.errors.append(f"'continue' outside loop at L{node.line}")
    
    def compile_expression(self, node: ASTNode):
        """Compile an expression"""
        if node is None:
            self.emit(OpCode.LOAD_NIL)
            return
        
        line = getattr(node, 'line', 0)
        
        if isinstance(node, NumberLiteral):
            const_idx = self.add_constant(node.value)
            self.emit(OpCode.LOAD_CONST, const_idx, line=line)
        
        elif isinstance(node, StringLiteral):
            const_idx = self.add_constant(node.value)
            self.emit(OpCode.LOAD_CONST, const_idx, line=line)
        
        elif isinstance(node, BooleanLiteral):
            if node.value:
                self.emit(OpCode.LOAD_TRUE, line=line)
            else:
                self.emit(OpCode.LOAD_FALSE, line=line)
        
        elif isinstance(node, NilLiteral):
            self.emit(OpCode.LOAD_NIL, line=line)
        
        elif isinstance(node, VarargLiteral):
            self.emit(OpCode.VARARG, line=line)
        
        elif isinstance(node, Identifier):
            self.compile_load_variable(node)
        
        elif isinstance(node, BinaryOp):
            self.compile_binary_op(node)
        
        elif isinstance(node, UnaryOp):
            self.compile_unary_op(node)
        
        elif isinstance(node, FunctionCall):
            self.compile_function_call(node)
        
        elif isinstance(node, IndexAccess):
            self.compile_expression(node.object)
            self.compile_expression(node.index)
            self.emit(OpCode.GET_TABLE, line=line)
        
        elif isinstance(node, MemberAccess):
            self.compile_expression(node.object)
            const_idx = self.add_constant(node.member)
            self.emit(OpCode.GET_FIELD, const_idx, line=line)
        
        elif isinstance(node, TableConstructor):
            self.compile_table(node)
        
        elif isinstance(node, FunctionDecl):
            # Anonymous function
            func = self.compile_function_body(node)
            self.ctx.function.children.append(func)
            child_idx = len(self.ctx.function.children) - 1
            self.emit(OpCode.CLOSURE, child_idx, line=line)
        
        else:
            self.errors.append(f"Unknown expression type: {type(node).__name__}")
            self.emit(OpCode.LOAD_NIL, line=line)
    
    def compile_load_variable(self, node: Identifier):
        """Compile loading a variable"""
        line = node.line
        name = node.name
        
        local_idx = self.ctx.find_local(name)
        if local_idx >= 0:
            self.emit(OpCode.LOAD_VAR, local_idx, line=line)
            return
        
        upval_idx = self.ctx.find_upvalue(name)
        if upval_idx >= 0:
            self.emit(OpCode.LOAD_UPVAL, upval_idx, line=line)
            return
        
        const_idx = self.add_constant(name)
        self.emit(OpCode.LOAD_GLOBAL, const_idx, line=line)
    
    def compile_binary_op(self, node: BinaryOp):
        """Compile binary operation"""
        line = node.line
        op = node.operator
        
        # Short-circuit operators
        if op == 'and':
            self.compile_expression(node.left)
            self.emit(OpCode.DUP, line=line)
            jump = self.emit(OpCode.JMP_IF_NOT, 0, line=line)
            self.emit(OpCode.POP, line=line)
            self.compile_expression(node.right)
            self.patch_jump(jump)
            return
        
        if op == 'or':
            self.compile_expression(node.left)
            self.emit(OpCode.DUP, line=line)
            jump = self.emit(OpCode.JMP_IF, 0, line=line)
            self.emit(OpCode.POP, line=line)
            self.compile_expression(node.right)
            self.patch_jump(jump)
            return
        
        # Regular binary operators
        self.compile_expression(node.left)
        self.compile_expression(node.right)
        
        op_map = {
            '+': OpCode.ADD, '-': OpCode.SUB, '*': OpCode.MUL,
            '/': OpCode.DIV, '%': OpCode.MOD, '^': OpCode.POW,
            '..': OpCode.CONCAT,
            '==': OpCode.EQ, '~=': OpCode.NE,
            '<': OpCode.LT, '<=': OpCode.LE,
            '>': OpCode.GT, '>=': OpCode.GE
        }
        
        if op in op_map:
            self.emit(op_map[op], line=line)
        else:
            self.errors.append(f"Unknown operator: {op}")
    
    def compile_unary_op(self, node: UnaryOp):
        """Compile unary operation"""
        line = node.line
        
        self.compile_expression(node.operand)
        
        if node.operator == '-':
            self.emit(OpCode.UNM, line=line)
        elif node.operator == 'not':
            self.emit(OpCode.NOT, line=line)
        elif node.operator == '#':
            self.emit(OpCode.LEN, line=line)
    
    def compile_function_call(self, node: FunctionCall):
        """Compile function call"""
        line = node.line
        
        if node.method:
            # Method call obj:method(args)
            self.compile_expression(node.callee)
            self.emit(OpCode.DUP, line=line)  # Duplicate for self
            const_idx = self.add_constant(node.method)
            self.emit(OpCode.GET_FIELD, const_idx, line=line)
            
            # Compile arguments
            for arg in node.args:
                self.compile_expression(arg)
            
            self.emit(OpCode.SELF, len(node.args), line=line)
        else:
            # Regular call func(args)
            self.compile_expression(node.callee)
            
            for arg in node.args:
                self.compile_expression(arg)
            
            self.emit(OpCode.CALL, len(node.args), line=line)
    
    def compile_table(self, node: TableConstructor):
        """Compile table constructor"""
        line = node.line
        
        self.emit(OpCode.NEW_TABLE, len(node.fields), line=line)
        
        array_idx = 1
        for field in node.fields:
            self.emit(OpCode.DUP, line=line)  # Duplicate table reference
            
            if field.is_array:
                # Array element: t[n] = value
                self.emit(OpCode.LOAD_CONST, self.add_constant(array_idx), line=line)
                array_idx += 1
            else:
                # Keyed element: t[key] = value
                self.compile_expression(field.key)
            
            self.compile_expression(field.value)
            self.emit(OpCode.SET_TABLE, line=line)


def compile_lua(source: str) -> Tuple[CompiledFunction, List[str]]:
    """
    Compile Lua source code to bytecode
    
    Args:
        source: Lua source code
        
    Returns:
        Tuple of (CompiledFunction, errors)
    """
    from .lexer import LuaLexer
    from .parser import LuaParser
    
    lexer = LuaLexer(source)
    tokens = lexer.tokenize()
    
    parser = LuaParser(tokens)
    ast = parser.parse()
    
    compiler = BytecodeCompiler()
    func, errors = compiler.compile(ast)
    
    errors = lexer.get_errors() + parser.get_errors() + errors
    return func, errors
