import random
import string
import hashlib
import struct
import base64
import zlib
from typing import List, Dict, Any, Optional, Tuple
from .compiler import CompiledFunction, OpCode, Instruction

class VMGenerator:
    """
    Professional VM Generator
    Generates complex, Luraph-style obfuscated output
    """
    
    def __init__(self, key: bytes = None, use_encryption: bool = True):
        self.key = key or bytes([random.randint(0, 255) for _ in range(32)])
        self.use_encryption = use_encryption
        self.opcode_map: Dict[int, int] = {}
        self.reverse_map: Dict[int, int] = {}
        self.string_keys: List[int] = [random.randint(1, 255) for _ in range(16)]
        self.var_counter = 0
        self.generated_names: set = set()
        
        # Generate complex opcode mapping
        self._generate_opcode_mapping()
        
        # Generate decoder key
        self.decoder_seed = random.randint(100000, 999999)
    
    def _generate_opcode_mapping(self):
        """Generate randomized opcode mapping"""
        available = list(range(1, 250))
        random.shuffle(available)
        
        for op in OpCode:
            if available:
                mapped = available.pop()
                self.opcode_map[op.value] = mapped
                self.reverse_map[mapped] = op.value
    
    def _random_var(self, length: int = None) -> str:
        """Generate unique random variable name"""
        if length is None:
            length = random.randint(6, 12)
        
        while True:
            styles = [
                self._gen_mixed_case,
                self._gen_underscore_heavy,
                self._gen_similar_chars,
                self._gen_hex_style,
            ]
            name = random.choice(styles)(length)
            if name not in self.generated_names:
                self.generated_names.add(name)
                return name
    
    def _gen_mixed_case(self, length: int) -> str:
        chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_'
        first = random.choice('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_')
        rest = ''.join(random.choice(chars + '0123456789') for _ in range(length - 1))
        return first + rest
    
    def _gen_underscore_heavy(self, length: int) -> str:
        chars = '_lIi'
        first = '_'
        rest = ''.join(random.choice(chars) for _ in range(length - 2))
        return first + rest + str(random.randint(0, 99))
    
    def _gen_similar_chars(self, length: int) -> str:
        chars = 'lI1O0oiLlIi'
        first = random.choice('lILi')
        rest = ''.join(random.choice(chars) for _ in range(length - 2))
        return first + rest + random.choice('aAbBcC')
    
    def _gen_hex_style(self, length: int) -> str:
        return '_0x' + ''.join(random.choice('0123456789abcdef') for _ in range(length - 3))
    
    def _encode_number(self, num: int) -> str:
        """Encode number in random format (hex, binary, octal, decimal)"""
        if num < 0:
            return str(num)
        
        formats = [
            lambda n: str(n),  # decimal
            lambda n: f"0x{n:X}" if n > 0 else "0x0",  # hex upper
            lambda n: f"0x{n:x}" if n > 0 else "0x0",  # hex lower
            lambda n: f"0B{bin(n)[2:]}" if n >= 0 else str(n),  # binary
            lambda n: f"0b{bin(n)[2:]}" if n >= 0 else str(n),  # binary lower
        ]
        
        if num == 0:
            return random.choice(["0", "0x0", "0X0", "0B0", "0b0"])
        
        return random.choice(formats)(num)
    
    def _encode_string(self, s: str) -> Tuple[List[int], List[int]]:
        """Encode string with XOR encryption"""
        key = [random.randint(1, 255) for _ in range(max(8, len(s) // 4))]
        encoded = []
        for i, char in enumerate(s):
            encoded.append(ord(char) ^ key[i % len(key)])
        return encoded, key
    
    def _generate_string_decoder(self) -> str:
        """Generate string decryption function"""
        fn = self._random_var()
        k = self._random_var()
        d = self._random_var()
        r = self._random_var()
        i = self._random_var()
        b = self._random_var()
        
        return f'''
local {fn}=(function({k},{d})
local {r}={{}};
for {i}=1,#{d} do
local {b}={d}[{i}];
{r}[{i}]=string.char(({b}~{k}[({i}-1)%#{k}+1]));
end;
return table.concat({r});
end);'''
    
    def _serialize_constants(self, constants: List[Any]) -> str:
        """Serialize constants with encoding"""
        parts = []
        
        for const in constants:
            if const is None:
                parts.append("nil")
            elif isinstance(const, bool):
                parts.append("true" if const else "false")
            elif isinstance(const, int):
                parts.append(self._encode_number(const))
            elif isinstance(const, float):
                if const == float('inf'):
                    parts.append("(1/0)")
                elif const == float('-inf'):
                    parts.append("(-1/0)")
                elif const != const:  # NaN
                    parts.append("(0/0)")
                else:
                    parts.append(repr(const))
            elif isinstance(const, str):
                # Encode string
                encoded, key = self._encode_string(const)
                enc_str = ','.join(self._encode_number(b) for b in encoded)
                key_str = ','.join(self._encode_number(k) for k in key)
                parts.append(f"_S({{{key_str}}},{{{enc_str}}})")
            else:
                parts.append("nil")
        
        return '{' + ','.join(parts) + '}'
    
    def _serialize_instructions(self, instructions: List[Instruction]) -> str:
        """Serialize instructions with encoding"""
        parts = []
        
        for instr in instructions:
            mapped_op = self.opcode_map.get(instr.opcode.value, instr.opcode.value)
            
            if instr.operand is not None:
                if instr.operand2 is not None:
                    parts.append(f"{{{self._encode_number(mapped_op)},{self._encode_number(instr.operand)},{self._encode_number(instr.operand2)}}}")
                else:
                    parts.append(f"{{{self._encode_number(mapped_op)},{self._encode_number(instr.operand)}}}")
            else:
                parts.append(f"{{{self._encode_number(mapped_op)}}}")
        
        return '{' + ','.join(parts) + '}'
    
    def _serialize_function(self, func: CompiledFunction) -> str:
        """Serialize function with all metadata"""
        children_parts = []
        for child in func.children:
            children_parts.append(self._serialize_function(child))
        
        children_str = '{' + ','.join(children_parts) + '}' if children_parts else '{}'
        
        return f'''{{
[{self._encode_number(1)}]={self._serialize_constants(func.constants)},
[{self._encode_number(2)}]={self._serialize_instructions(func.instructions)},
[{self._encode_number(3)}]={self._encode_number(len(func.params))},
[{self._encode_number(4)}]={'true' if func.is_vararg else 'false'},
[{self._encode_number(5)}]={children_str},
[{self._encode_number(6)}]={self._encode_number(len(func.locals))}
}}'''
    
    def _generate_vm_core(self) -> str:
        """Generate the core VM execution engine"""
        # Variable names
        v_exec = self._random_var()
        v_wrap = self._random_var()
        v_create = self._random_var()
        v_stk = self._random_var()
        v_top = self._random_var()
        v_pc = self._random_var()
        v_code = self._random_var()
        v_const = self._random_var()
        v_upvals = self._random_var()
        v_locals = self._random_var()
        v_vararg = self._random_var()
        v_nvar = self._random_var()
        v_instr = self._random_var()
        v_op = self._random_var()
        v_a = self._random_var()
        v_b = self._random_var()
        v_env = self._random_var()
        v_func = self._random_var()
        v_children = self._random_var()
        v_nparams = self._random_var()
        v_push = self._random_var()
        v_pop = self._random_var()
        v_peek = self._random_var()
        v_OP = self._random_var()
        
        # Opcode table
        op_table_parts = []
        for op in OpCode:
            mapped = self.opcode_map[op.value]
            op_table_parts.append(f"[{self._encode_number(mapped)}]={self._encode_number(op.value)}")
        
        op_table = '{' + ','.join(op_table_parts) + '}'
        
        return f'''
local {v_OP}={op_table};

local {v_create};
{v_create}=function({v_func},{v_env},{v_upvals})
{v_env}={v_env} or (getfenv and getfenv(0)) or _ENV or _G;

local {v_exec};
{v_exec}=function(...)
local {v_code}={v_func}[{self._encode_number(2)}];
local {v_const}={v_func}[{self._encode_number(1)}];
local {v_nparams}={v_func}[{self._encode_number(3)}];
local {v_children}={v_func}[{self._encode_number(5)}];

local {v_stk}={{}};
local {v_top}={self._encode_number(0)};
local {v_pc}={self._encode_number(1)};
local {v_locals}={{}};
local {v_vararg}={{...}};
local {v_nvar}=select('#',...);

for _i=1,{v_nparams} do
{v_locals}[_i]={v_vararg}[_i];
end;

local function {v_push}(_v)
{v_top}={v_top}+1;
{v_stk}[{v_top}]=_v;
end;

local function {v_pop}()
local _v={v_stk}[{v_top}];
{v_stk}[{v_top}]=nil;
{v_top}={v_top}-1;
return _v;
end;

local function {v_peek}(_o)
return {v_stk}[{v_top}-(_o or 0)];
end;

while {v_pc}<=#({v_code}) do
local {v_instr}={v_code}[{v_pc}];
local {v_op}={v_OP}[{v_instr}[1]];
local {v_a}={v_instr}[2];
local {v_b}={v_instr}[3];
{v_pc}={v_pc}+1;

if {v_op}=={self._encode_number(OpCode.LOAD_CONST.value)} then
{v_push}({v_const}[{v_a}+1]);
elseif {v_op}=={self._encode_number(OpCode.LOAD_NIL.value)} then
{v_push}(nil);
elseif {v_op}=={self._encode_number(OpCode.LOAD_TRUE.value)} then
{v_push}(true);
elseif {v_op}=={self._encode_number(OpCode.LOAD_FALSE.value)} then
{v_push}(false);
elseif {v_op}=={self._encode_number(OpCode.LOAD_VAR.value)} then
{v_push}({v_locals}[{v_a}+1]);
elseif {v_op}=={self._encode_number(OpCode.STORE_VAR.value)} then
{v_locals}[{v_a}+1]={v_pop}();
elseif {v_op}=={self._encode_number(OpCode.LOAD_GLOBAL.value)} then
{v_push}({v_env}[{v_const}[{v_a}+1]]);
elseif {v_op}=={self._encode_number(OpCode.STORE_GLOBAL.value)} then
{v_env}[{v_const}[{v_a}+1]]={v_pop}();
elseif {v_op}=={self._encode_number(OpCode.LOAD_UPVAL.value)} then
{v_push}({v_upvals}[{v_a}+1]);
elseif {v_op}=={self._encode_number(OpCode.STORE_UPVAL.value)} then
{v_upvals}[{v_a}+1]={v_pop}();
elseif {v_op}=={self._encode_number(OpCode.NEW_TABLE.value)} then
{v_push}({{}});
elseif {v_op}=={self._encode_number(OpCode.GET_TABLE.value)} then
local _k={v_pop}();local _t={v_pop}();{v_push}(_t[_k]);
elseif {v_op}=={self._encode_number(OpCode.SET_TABLE.value)} then
local _v={v_pop}();local _k={v_pop}();local _t={v_pop}();_t[_k]=_v;
elseif {v_op}=={self._encode_number(OpCode.GET_FIELD.value)} then
local _t={v_pop}();{v_push}(_t[{v_const}[{v_a}+1]]);
elseif {v_op}=={self._encode_number(OpCode.SET_FIELD.value)} then
local _v={v_pop}();local _t={v_pop}();_t[{v_const}[{v_a}+1]]=_v;
elseif {v_op}=={self._encode_number(OpCode.ADD.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a+_b);
elseif {v_op}=={self._encode_number(OpCode.SUB.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a-_b);
elseif {v_op}=={self._encode_number(OpCode.MUL.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a*_b);
elseif {v_op}=={self._encode_number(OpCode.DIV.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a/_b);
elseif {v_op}=={self._encode_number(OpCode.MOD.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a%_b);
elseif {v_op}=={self._encode_number(OpCode.POW.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a^_b);
elseif {v_op}=={self._encode_number(OpCode.UNM.value)} then
{v_push}(-{v_pop}());
elseif {v_op}=={self._encode_number(OpCode.CONCAT.value)} then
local _b=tostring({v_pop}());local _a=tostring({v_pop}());{v_push}(_a.._b);
elseif {v_op}=={self._encode_number(OpCode.LEN.value)} then
{v_push}(#{v_pop}());
elseif {v_op}=={self._encode_number(OpCode.EQ.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a==_b);
elseif {v_op}=={self._encode_number(OpCode.NE.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a~=_b);
elseif {v_op}=={self._encode_number(OpCode.LT.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a<_b);
elseif {v_op}=={self._encode_number(OpCode.LE.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a<=_b);
elseif {v_op}=={self._encode_number(OpCode.GT.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a>_b);
elseif {v_op}=={self._encode_number(OpCode.GE.value)} then
local _b={v_pop}();local _a={v_pop}();{v_push}(_a>=_b);
elseif {v_op}=={self._encode_number(OpCode.NOT.value)} then
{v_push}(not {v_pop}());
elseif {v_op}=={self._encode_number(OpCode.JMP.value)} then
{v_pc}={v_a}+1;
elseif {v_op}=={self._encode_number(OpCode.JMP_IF.value)} then
if {v_peek}() then {v_pc}={v_a}+1;end;
elseif {v_op}=={self._encode_number(OpCode.JMP_IF_NOT.value)} then
if not {v_pop}() then {v_pc}={v_a}+1;end;
elseif {v_op}=={self._encode_number(OpCode.CALL.value)} then
local _n={v_a};local _args={{}};
for _i=_n,1,-1 do _args[_i]={v_pop}();end;
local _f={v_pop}();
local _r={{_f(unpack(_args))}};
for _,_v in ipairs(_r) do {v_push}(_v);end;
elseif {v_op}=={self._encode_number(OpCode.SELF.value)} then
local _n={v_a};local _args={{}};
for _i=_n,1,-1 do _args[_i]={v_pop}();end;
local _m={v_pop}();local _s={v_pop}();
local _r={{_m(_s,unpack(_args))}};
for _,_v in ipairs(_r) do {v_push}(_v);end;
elseif {v_op}=={self._encode_number(OpCode.RETURN.value)} then
local _n={v_a};local _r={{}};
for _i=_n,1,-1 do _r[_i]={v_pop}();end;
return unpack(_r);
elseif {v_op}=={self._encode_number(OpCode.CLOSURE.value)} then
local _cf={v_children}[{v_a}+1];
local _cl={v_create}(_cf,{v_env},{v_locals});
{v_push}(_cl);
elseif {v_op}=={self._encode_number(OpCode.VARARG.value)} then
for _i={v_nparams}+1,{v_nvar} do {v_push}({v_vararg}[_i]);end;
elseif {v_op}=={self._encode_number(OpCode.DUP.value)} then
{v_push}({v_peek}());
elseif {v_op}=={self._encode_number(OpCode.POP.value)} then
{v_pop}();
elseif {v_op}=={self._encode_number(OpCode.FORPREP.value)} then
local _s={v_pop}();local _l={v_pop}();local _i={v_pop}();
{v_locals}[{v_a}+1]=_i-_s;
{v_push}(_i);{v_push}(_l);{v_push}(_s);
elseif {v_op}=={self._encode_number(OpCode.FORLOOP.value)} then
local _s={v_peek}(0);local _l={v_peek}(1);
local _i={v_locals}[{v_a}+1]+_s;
{v_locals}[{v_a}+1]=_i;
if (_s>0 and _i<=_l) or (_s<=0 and _i>=_l) then
{v_pc}={v_a};
else
{v_pop}();{v_pop}();{v_pop}();
end;
elseif {v_op}=={self._encode_number(OpCode.TFORLOOP.value)} then
local _n={v_a};
local _it={v_peek}(2);local _st={v_peek}(1);local _ct={v_peek}(0);
local _r={{_it(_st,_ct)}};
if _r[1]==nil then
{v_pc}={v_a}+1;
else
for _i=1,_n do {v_locals}[_i]=_r[_i];end;
{v_stk}[{v_top}]=_r[1];
end;
end;
end;
return nil;
end;

return {v_exec};
end;

return {v_create};'''
    
    def _generate_wrapper_layers(self, core: str, bytecode_str: str) -> str:
        """Generate multiple wrapper layers for protection"""
        v_main = self._random_var()
        v_bc = self._random_var()
        v_vm = self._random_var()
        v_run = self._random_var()
        v_s = self._random_var()
        v_ret = self._random_var()
        
        # String decoder function
        str_decoder = self._generate_string_decoder()
        str_fn = str_decoder.split('=')[0].replace('local ', '').strip()
        
        return f'''-- This file was obfuscated using LuaShield VM Obfuscator
-- https://luashield.dev | Professional Lua Protection

local {v_s}=string;local {v_ret}={{
d=coroutine.yield,
Q={v_s}.byte,
R=function(...)(...)[...]=nil;end,
g=function(_K,_U)
_K[0B10101]=(function(_a,_b,_c)
local _D={{_K[0B10101]}};
if not(_b>_a)then else return;end;
local _Z=(_a-_b+0X1);
if _Z>=0X8 then 
return _c[_b],_c[_b+1],_c[_b+0X2],_c[_b+0B11],_c[_b+4],_c[_b+5],_c[_b+0B110],_c[_b+0X07],_D[1](_a,_b+8,_c);
else 
return _c[_b],_D[1](_a,_b+1,_c);
end;
end);
(_K)[{self._encode_number(22)}]=(select);
_K[{self._encode_number(23)}]=nil;
end,
e=coroutine.wrap,
t={v_s}.sub,
UU={v_s}.gsub,
JU=bit32 and bit32.bnot or function(_x)return~_x;end,
q=bit32 and bit32.bor or function(_a,_b)return _a|_b;end,
X={v_s}.match,
I={v_s}.unpack or unpack,
a=table.move or function(_t,_a,_b,_c,_d)for _i=_a,_b do _d[_c+_i-_a]=_t[_i];end;return _d;end,
}};

{str_decoder}

local _S={str_fn};

local {v_bc}={bytecode_str};

local {v_vm}=(function()
{core}
end)();

local {v_main}={v_vm}({v_bc});

return {v_main}(...);'''
    
    def _generate_anti_analysis(self) -> str:
        """Generate anti-analysis code"""
        v1 = self._random_var()
        v2 = self._random_var()
        v3 = self._random_var()
        
        checks = []
        
        # Environment check
        checks.append(f'''
(function()
local {v1}=_G or _ENV;
local {v2}={{pcall,error,type,pairs}};
local {v3}=(function(_t)
for _k,_v in pairs(_t) do
if type(_v)~="function" then return false;end;
end;
return true;
end)({v2});
if not {v3} then return;end;
end)();''')
        
        return '\n'.join(checks)
    
    def generate(self, func: CompiledFunction, mode: str = "loadstring") -> str:
        """Generate complete obfuscated VM code"""
        # Serialize bytecode
        bytecode_str = self._serialize_function(func)
        
        # Generate VM core
        vm_core = self._generate_vm_core()
        
        # Generate anti-analysis
        anti_analysis = self._generate_anti_analysis()
        
        # Wrap everything
        output = self._generate_wrapper_layers(vm_core, bytecode_str)
        
        # Add anti-analysis
        output = anti_analysis + '\n' + output
        
        return output
