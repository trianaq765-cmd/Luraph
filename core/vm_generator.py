import random
import string
import json
from typing import List, Dict, Any, Optional
from .compiler import CompiledFunction, OpCode, Instruction

class VMGenerator:
    """
    VM Generator - Generates Lua VM runtime with embedded bytecode
    Supports: Roblox Lua, loadstring, raw mode
    """
    
    def __init__(self, key: bytes = None, use_encryption: bool = True):
        self.key = key or self.generate_key()
        self.use_encryption = use_encryption
        self.opcode_map: Dict[int, int] = {}
        self.reverse_map: Dict[int, int] = {}
        self.var_prefix = self.random_name(4)
        self.generate_opcode_mapping()
        
    def generate_key(self, length: int = 32) -> bytes:
        """Generate random encryption key"""
        return bytes([random.randint(0, 255) for _ in range(length)])
    
    def generate_opcode_mapping(self):
        """Generate randomized opcode mapping for obfuscation"""
        opcodes = list(range(1, 256))  # Avoid 0
        random.shuffle(opcodes)
        
        for original_op in OpCode:
            if opcodes:
                mapped = opcodes.pop()
                self.opcode_map[original_op.value] = mapped
                self.reverse_map[mapped] = original_op.value
    
    def random_name(self, length: int = 8) -> str:
        """Generate random variable name"""
        chars = string.ascii_letters + '_'
        first = random.choice(string.ascii_letters + '_')
        rest = ''.join(random.choice(chars + string.digits) for _ in range(length - 1))
        return first + rest
    
    def encode_string(self, s: str) -> str:
        """Encode string for safe Lua embedding"""
        result = []
        for char in s:
            code = ord(char)
            if code < 32 or code > 126 or char in '\\"\n\r\t':
                result.append(f'\\{code}')
            elif char == '"':
                result.append('\\"')
            elif char == '\\':
                result.append('\\\\')
            else:
                result.append(char)
        return ''.join(result)
    
    def serialize_value(self, value: Any, indent: int = 0) -> str:
        """Serialize Python value to Lua literal"""
        spaces = "    " * indent
        
        if value is None:
            return "nil"
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, int):
            return str(value)
        elif isinstance(value, float):
            if value != value:  # NaN
                return "(0/0)"
            elif value == float('inf'):
                return "(1/0)"
            elif value == float('-inf'):
                return "(-1/0)"
            return repr(value)
        elif isinstance(value, str):
            return f'"{self.encode_string(value)}"'
        elif isinstance(value, (list, tuple)):
            if not value:
                return "{}"
            items = []
            for i, item in enumerate(value):
                serialized = self.serialize_value(item, indent + 1)
                items.append(f"{spaces}    [{i+1}] = {serialized}")
            return "{\n" + ",\n".join(items) + f"\n{spaces}}}"
        elif isinstance(value, dict):
            if not value:
                return "{}"
            items = []
            for k, v in value.items():
                key_str = self.serialize_key(k)
                val_str = self.serialize_value(v, indent + 1)
                items.append(f"{spaces}    {key_str} = {val_str}")
            return "{\n" + ",\n".join(items) + f"\n{spaces}}}"
        return "nil"
    
    def serialize_key(self, key: Any) -> str:
        """Serialize dictionary key for Lua table"""
        if isinstance(key, str):
            if key.isidentifier() and not self.is_lua_keyword(key):
                return key
            return f'["{self.encode_string(key)}"]'
        elif isinstance(key, int):
            return f"[{key}]"
        return f"[{self.serialize_value(key)}]"
    
    def is_lua_keyword(self, name: str) -> bool:
        """Check if name is Lua keyword"""
        keywords = {'and', 'break', 'do', 'else', 'elseif', 'end', 'false',
                   'for', 'function', 'if', 'in', 'local', 'nil', 'not',
                   'or', 'repeat', 'return', 'then', 'true', 'until', 'while'}
        return name in keywords
    
    def serialize_function(self, func: CompiledFunction) -> Dict[str, Any]:
        """Serialize compiled function to dictionary"""
        serialized_instructions = []
        for instr in func.instructions:
            mapped_op = self.opcode_map.get(instr.opcode.value, instr.opcode.value)
            instr_data = {'o': mapped_op}
            if instr.operand is not None:
                instr_data['a'] = instr.operand
            serialized_instructions.append(instr_data)
        
        serialized_children = []
        for child in func.children:
            serialized_children.append(self.serialize_function(child))
        
        result = {
            'n': func.name or '',
            'p': func.params,
            'v': func.is_vararg,
            'i': serialized_instructions,
            'c': func.constants,
            'l': func.locals,
            'u': func.upvalues,
            'f': serialized_children
        }
        
        return result
    
    def generate_opcode_table(self) -> str:
        """Generate Lua opcode mapping table"""
        # Create reverse lookup: mapped_code -> operation name
        lines = [f"local {self.var_prefix}_OP = {{"]
        for op in OpCode:
            mapped = self.opcode_map[op.value]
            lines.append(f'    [{mapped}] = "{op.name}",')
        lines.append("}")
        return '\n'.join(lines)
    
    def generate_vm_runtime(self) -> str:
        """Generate the Lua VM runtime code"""
        v = self.var_prefix
        
        vm_code = f'''
-- LuaShield VM v2.1
-- Protected Lua Virtual Machine

local {v}_bit = bit32 or bit or {{
    band = function(a,b) return a & b end,
    bor = function(a,b) return a | b end,
    bxor = function(a,b) return a ~ b end,
    bnot = function(a) return ~a end,
    lshift = function(a,b) return a << b end,
    rshift = function(a,b) return a >> b end
}}

local {v}_band = {v}_bit.band
local {v}_bxor = {v}_bit.bxor
local {v}_unpack = unpack or table.unpack
local {v}_select = select
local {v}_type = type
local {v}_tostring = tostring
local {v}_tonumber = tonumber
local {v}_pairs = pairs
local {v}_ipairs = ipairs
local {v}_next = next
local {v}_pcall = pcall
local {v}_error = error
local {v}_setmetatable = setmetatable
local {v}_getmetatable = getmetatable
local {v}_rawget = rawget
local {v}_rawset = rawset
local {v}_concat = table.concat
local {v}_insert = table.insert

'''
        vm_code += self.generate_opcode_table()
        vm_code += f'''

local function {v}_VM(bytecode, env)
    env = env or (getfenv and getfenv(0)) or _ENV or _G
    
    local function execute(func, upvals, ...)
        local code = func.i
        local consts = func.c
        local nparams = #func.p
        local isVararg = func.v
        local children = func.f
        local OP = {v}_OP
        
        local locals = {{}}
        local stack = {{}}
        local top = 0
        local pc = 1
        local varargs = {{...}}
        local nvarargs = {v}_select('#', ...)
        
        -- Initialize parameters
        for i = 1, nparams do
            locals[i] = varargs[i]
        end
        
        local function push(val)
            top = top + 1
            stack[top] = val
        end
        
        local function pop()
            local val = stack[top]
            stack[top] = nil
            top = top - 1
            return val
        end
        
        local function peek(offset)
            return stack[top - (offset or 0)]
        end
        
        local function popn(n)
            local results = {{}}
            for i = n, 1, -1 do
                results[i] = pop()
            end
            return results
        end
        
        while pc <= #code do
            local instr = code[pc]
            local op = OP[instr.o]
            local arg = instr.a
            pc = pc + 1
            
            -- Constants & Literals
            if op == "LOAD_CONST" then
                push(consts[arg + 1])
            elseif op == "LOAD_NIL" then
                push(nil)
            elseif op == "LOAD_TRUE" then
                push(true)
            elseif op == "LOAD_FALSE" then
                push(false)
                
            -- Variables
            elseif op == "LOAD_VAR" then
                push(locals[arg + 1])
            elseif op == "STORE_VAR" then
                locals[arg + 1] = pop()
            elseif op == "LOAD_GLOBAL" then
                push(env[consts[arg + 1]])
            elseif op == "STORE_GLOBAL" then
                env[consts[arg + 1]] = pop()
            elseif op == "LOAD_UPVAL" then
                push(upvals[arg + 1])
            elseif op == "STORE_UPVAL" then
                upvals[arg + 1] = pop()
                
            -- Tables
            elseif op == "NEW_TABLE" then
                push({{}})
            elseif op == "GET_TABLE" then
                local idx = pop()
                local tbl = pop()
                push(tbl[idx])
            elseif op == "SET_TABLE" then
                local val = pop()
                local idx = pop()
                local tbl = pop()
                tbl[idx] = val
            elseif op == "GET_FIELD" then
                local name = consts[arg + 1]
                push(pop()[name])
            elseif op == "SET_FIELD" then
                local name = consts[arg + 1]
                local val = pop()
                pop()[name] = val
                
            -- Arithmetic
            elseif op == "ADD" then
                local b, a = pop(), pop()
                push(a + b)
            elseif op == "SUB" then
                local b, a = pop(), pop()
                push(a - b)
            elseif op == "MUL" then
                local b, a = pop(), pop()
                push(a * b)
            elseif op == "DIV" then
                local b, a = pop(), pop()
                push(a / b)
            elseif op == "MOD" then
                local b, a = pop(), pop()
                push(a % b)
            elseif op == "POW" then
                local b, a = pop(), pop()
                push(a ^ b)
            elseif op == "UNM" then
                push(-pop())
            elseif op == "CONCAT" then
                local b, a = pop(), pop()
                push({v}_tostring(a) .. {v}_tostring(b))
            elseif op == "LEN" then
                push(#pop())
                
            -- Comparison
            elseif op == "EQ" then
                local b, a = pop(), pop()
                push(a == b)
            elseif op == "NE" then
                local b, a = pop(), pop()
                push(a ~= b)
            elseif op == "LT" then
                local b, a = pop(), pop()
                push(a < b)
            elseif op == "LE" then
                local b, a = pop(), pop()
                push(a <= b)
            elseif op == "GT" then
                local b, a = pop(), pop()
                push(a > b)
            elseif op == "GE" then
                local b, a = pop(), pop()
                push(a >= b)
                
            -- Logical
            elseif op == "NOT" then
                push(not pop())
                
            -- Control Flow
            elseif op == "JMP" then
                pc = arg + 1
            elseif op == "JMP_IF" then
                if peek() then
                    pc = arg + 1
                end
            elseif op == "JMP_IF_NOT" then
                if not pop() then
                    pc = arg + 1
                end
                
            -- Functions
            elseif op == "CALL" then
                local nargs = arg
                local args = popn(nargs)
                local fn = pop()
                local results = {{fn({v}_unpack(args))}}
                for _, v in {v}_ipairs(results) do
                    push(v)
                end
            elseif op == "SELF" then
                local nargs = arg
                local args = popn(nargs)
                local method = pop()
                local self = pop()
                local results = {{method(self, {v}_unpack(args))}}
                for _, v in {v}_ipairs(results) do
                    push(v)
                end
            elseif op == "RETURN" then
                local nvals = arg
                local results = popn(nvals)
                return {v}_unpack(results)
            elseif op == "CLOSURE" then
                local childFunc = children[arg + 1]
                local closure = function(...)
                    return execute(childFunc, locals, ...)
                end
                push(closure)
            elseif op == "VARARG" then
                for i = nparams + 1, nvarargs do
                    push(varargs[i])
                end
                
            -- Stack
            elseif op == "DUP" then
                push(peek())
            elseif op == "POP" then
                pop()
            elseif op == "MOVE" then
                -- Move value
                
            -- For Loops
            elseif op == "FORPREP" then
                local step = pop()
                local limit = pop()
                local init = pop()
                locals[arg + 1] = init - step
                push(init)
                push(limit)
                push(step)
            elseif op == "FORLOOP" then
                local step = peek(0)
                local limit = peek(1)
                local idx = locals[arg + 1] + step
                locals[arg + 1] = idx
                
                local continue_loop = false
                if step > 0 then
                    continue_loop = idx <= limit
                else
                    continue_loop = idx >= limit
                end
                
                if continue_loop then
                    pc = arg
                else
                    pop() pop() pop()
                end
            elseif op == "TFORLOOP" then
                local nvals = arg
                local iter = peek(2)
                local state = peek(1)
                local ctrl = peek(0)
                local results = {{iter(state, ctrl)}}
                
                if results[1] == nil then
                    pc = arg + 1
                else
                    for i = 1, nvals do
                        locals[i] = results[i]
                    end
                    stack[top] = results[1]
                end
            elseif op == "NOP" then
                -- No operation
            end
        end
        
        return nil
    end
    
    return function(...)
        return execute(bytecode, {{}}, ...)
    end
end

return {v}_VM
'''
        return vm_code
    
    def generate_bytecode_loader(self, serialized: Dict[str, Any]) -> str:
        """Generate Lua code to load serialized bytecode"""
        lua_repr = self.serialize_value(serialized)
        return f"local {self.var_prefix}_bytecode = {lua_repr}"
    
    def generate(self, func: CompiledFunction, mode: str = "loadstring") -> str:
        """
        Generate complete obfuscated VM code
        
        Args:
            func: Compiled function
            mode: "loadstring" for loadstring support, "raw" for direct execution
        """
        serialized = self.serialize_function(func)
        v = self.var_prefix
        
        output = []
        output.append("-- Protected by LuaShield VM Obfuscator")
        output.append("-- Supports: Lua 5.1/5.2/5.3, Roblox Luau")
        output.append("-- Input: .lua or .txt files (NO .luac bytecode)")
        output.append("")
        output.append(self.generate_vm_runtime())
        output.append("")
        output.append(self.generate_bytecode_loader(serialized))
        output.append("")
        output.append(f"local {v}_main = {v}_VM({v}_bytecode)")
        
        if mode == "loadstring":
            output.append(f"return {v}_main(...)")
        else:  # raw mode
            output.append(f"{v}_main(...)")
        
        return '\n'.join(output)
