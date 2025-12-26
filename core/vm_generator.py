import random
import string
import hashlib
import base64
from typing import List, Dict, Any, Optional, Tuple
from .compiler import CompiledFunction, OpCode, Instruction

class VMGenerator:
    """
    Professional Dense VM Generator
    Output: Luraph/Moonsec style - DENSE, no sparse lines
    Target ratio: 600-800%
    """
    
    def __init__(self, key: bytes = None, use_encryption: bool = True):
        self.key = key or bytes([random.randint(0, 255) for _ in range(32)])
        self.use_encryption = use_encryption
        self.opcode_map: Dict[int, int] = {}
        self.generated_names: set = set()
        self.name_counter = 0
        self._generate_opcode_mapping()
    
    def _generate_opcode_mapping(self):
        """Generate randomized opcode mapping"""
        available = list(range(1, 200))
        random.shuffle(available)
        for op in OpCode:
            if available:
                self.opcode_map[op.value] = available.pop()
    
    def _rand_name(self, length: int = None) -> str:
        """Generate random variable name"""
        if length is None:
            length = random.randint(1, 4)
        
        self.name_counter += 1
        styles = [
            lambda: ''.join(random.choice('lIi1O0oQqCcKkSsZzXxVvWwYy') for _ in range(length)),
            lambda: '_' + ''.join(random.choice('abcdefghijklmnopqrstuvwxyz') for _ in range(length)),
            lambda: ''.join(random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ') for _ in range(length)),
            lambda: '_0x' + ''.join(random.choice('0123456789abcdef') for _ in range(length)),
        ]
        
        while True:
            name = random.choice(styles)() + str(self.name_counter % 100)
            if name not in self.generated_names and not name[0].isdigit():
                self.generated_names.add(name)
                return name
    
    def _encode_num(self, n: int) -> str:
        """Encode number in various formats"""
        if n < 0:
            return str(n)
        if n == 0:
            return random.choice(['0', '0x0', '0X0', '(0)'])
        
        formats = [
            str(n),
            f'0x{n:X}' if n < 65536 else str(n),
            f'0x{n:x}' if n < 65536 else str(n),
            f'0B{bin(n)[2:]}' if n < 256 else str(n),
            f'({n})',
            f'(0x{n:X})' if n < 4096 else str(n),
        ]
        return random.choice(formats)
    
    def _encode_string(self, s: str) -> Tuple[str, str]:
        """Encode string with XOR, return (encoded_data, key_array)"""
        if not s:
            return '""', '{}'
        
        key = [random.randint(1, 254) for _ in range(max(4, min(16, len(s) // 2)))]
        encoded = []
        for i, char in enumerate(s):
            encoded.append(ord(char) ^ key[i % len(key)])
        
        enc_str = ','.join(str(b) for b in encoded)
        key_str = ','.join(str(k) for k in key)
        return f'{{{enc_str}}}', f'{{{key_str}}}'
    
    def _make_dense(self, code: str, max_line_length: int = 200) -> str:
        """Make code dense - fill lines to max length"""
        # Remove all newlines and extra spaces
        code = ' '.join(code.split())
        
        # Split into chunks of max_line_length
        lines = []
        current_line = ""
        
        tokens = code.replace(';', '; ').replace('then', 'then ').replace('do', 'do ').replace('end', 'end ').split(' ')
        
        for token in tokens:
            if not token:
                continue
            
            if len(current_line) + len(token) + 1 > max_line_length:
                if current_line:
                    lines.append(current_line)
                current_line = token
            else:
                if current_line:
                    current_line += ' ' + token
                else:
                    current_line = token
        
        if current_line:
            lines.append(current_line)
        
        return '\n'.join(lines)
    
    def _serialize_constants(self, constants: List[Any]) -> str:
        """Serialize constants densely"""
        parts = []
        self._const_keys = []  # Store keys for string decryption
        
        for i, const in enumerate(constants):
            if const is None:
                parts.append('nil')
            elif isinstance(const, bool):
                parts.append('true' if const else 'false')
            elif isinstance(const, int):
                parts.append(self._encode_num(const))
            elif isinstance(const, float):
                if const == float('inf'):
                    parts.append('(1/0)')
                elif const == float('-inf'):
                    parts.append('(-1/0)')
                elif const != const:
                    parts.append('(0/0)')
                else:
                    parts.append(str(const))
            elif isinstance(const, str):
                enc_data, enc_key = self._encode_string(const)
                parts.append(f'_d({enc_key},{enc_data})')
            else:
                parts.append('nil')
        
        return '{' + ','.join(parts) + '}'
    
    def _serialize_instructions(self, instructions: List[Instruction]) -> str:
        """Serialize instructions densely"""
        parts = []
        for instr in instructions:
            op = self.opcode_map.get(instr.opcode.value, instr.opcode.value)
            if instr.operand is not None:
                parts.append(f'{{{self._encode_num(op)},{self._encode_num(instr.operand)}}}')
            else:
                parts.append(f'{{{self._encode_num(op)}}}')
        return '{' + ','.join(parts) + '}'
    
    def _serialize_function(self, func: CompiledFunction) -> str:
        """Serialize function with all data"""
        children = '{}'
        if func.children:
            child_parts = [self._serialize_function(c) for c in func.children]
            children = '{' + ','.join(child_parts) + '}'
        
        return (f'{{{self._serialize_constants(func.constants)},'
                f'{self._serialize_instructions(func.instructions)},'
                f'{len(func.params)},{1 if func.is_vararg else 0},{children}}}')
    
    def _generate_dense_vm(self) -> str:
        """Generate dense VM runtime - Luraph style"""
        # Short names for everything
        names = {k: self._rand_name(2) for k in [
            'env', 'stk', 'top', 'pc', 'code', 'const', 'loc', 'varg', 'nv',
            'child', 'np', 'op', 'a', 'b', 'i', 'up', 'push', 'pop', 'peek',
            'exec', 'create', 'wrap', 'run', 'decode', 'tbl', 'res', 'args',
            'fn', 'val', 'key', 'obj', 'idx', 'iter', 'state', 'ctrl', 'step',
            'limit', 'init', 'cnt', 'tmp', 'err'
        ]}
        
        # Opcodes
        ops = self.opcode_map
        
        # Build VM as single dense line
        vm_parts = []
        
        # Header utilities
        vm_parts.append(f"local {names['decode']}=function({names['key']},{names['val']})")
        vm_parts.append(f"local {names['res']}={{}}for {names['i']}=1,#{names['val']} do ")
        vm_parts.append(f"{names['res']}[{names['i']}]=string.char(bit32 and bit32.bxor({names['val']}[{names['i']}],{names['key']}[({names['i']}-1)%#{names['key']}+1]) or ({names['val']}[{names['i']}]~{names['key']}[({names['i']}-1)%#{names['key']}+1]))")
        vm_parts.append(f"end return table.concat({names['res']})end;")
        
        # Alias
        vm_parts.append(f"local _d={names['decode']};")
        vm_parts.append(f"local _u=unpack or table.unpack;")
        vm_parts.append(f"local _s=select;")
        vm_parts.append(f"local _p=pairs;")
        vm_parts.append(f"local _i=ipairs;")
        vm_parts.append(f"local _t=type;")
        vm_parts.append(f"local _ts=tostring;")
        vm_parts.append(f"local _tn=tonumber;")
        
        # Main VM function
        vm_parts.append(f"local {names['create']};")
        vm_parts.append(f"{names['create']}=function({names['fn']},{names['env']},{names['up']})")
        vm_parts.append(f"{names['env']}={names['env']} or (getfenv and getfenv(0)) or _ENV or _G;")
        
        # Execute function
        vm_parts.append(f"local {names['exec']};")
        vm_parts.append(f"{names['exec']}=function(...)")
        vm_parts.append(f"local {names['const']}={names['fn']}[1];")
        vm_parts.append(f"local {names['code']}={names['fn']}[2];")
        vm_parts.append(f"local {names['np']}={names['fn']}[3];")
        vm_parts.append(f"local {names['child']}={names['fn']}[5];")
        vm_parts.append(f"local {names['varg']}={{...}};")
        vm_parts.append(f"local {names['nv']}=_s('#',...);")
        vm_parts.append(f"local {names['stk']}={{}};")
        vm_parts.append(f"local {names['top']}=0;")
        vm_parts.append(f"local {names['pc']}=1;")
        vm_parts.append(f"local {names['loc']}={{}};")
        vm_parts.append(f"for {names['i']}=1,{names['np']} do {names['loc']}[{names['i']}]={names['varg']}[{names['i']}] end;")
        
        # Stack ops
        vm_parts.append(f"local function {names['push']}({names['val']}){names['top']}={names['top']}+1;{names['stk']}[{names['top']}]={names['val']};end;")
        vm_parts.append(f"local function {names['pop']}()local {names['val']}={names['stk']}[{names['top']}];{names['stk']}[{names['top']}]=nil;{names['top']}={names['top']}-1;return {names['val']};end;")
        vm_parts.append(f"local function {names['peek']}({names['idx']})return {names['stk']}[{names['top']}-({names['idx']} or 0)];end;")
        
        # Main loop
        vm_parts.append(f"while {names['pc']}<=#({names['code']}) do ")
        vm_parts.append(f"local {names['op']}={names['code']}[{names['pc']}][1];")
        vm_parts.append(f"local {names['a']}={names['code']}[{names['pc']}][2];")
        vm_parts.append(f"{names['pc']}={names['pc']}+1;")
        
        # Opcode handlers - all in one dense block
        handlers = [
            (OpCode.LOAD_CONST, f"if {names['op']}=={ops[OpCode.LOAD_CONST.value]} then {names['push']}({names['const']}[{names['a']}+1])"),
            (OpCode.LOAD_NIL, f"elseif {names['op']}=={ops[OpCode.LOAD_NIL.value]} then {names['push']}(nil)"),
            (OpCode.LOAD_TRUE, f"elseif {names['op']}=={ops[OpCode.LOAD_TRUE.value]} then {names['push']}(true)"),
            (OpCode.LOAD_FALSE, f"elseif {names['op']}=={ops[OpCode.LOAD_FALSE.value]} then {names['push']}(false)"),
            (OpCode.LOAD_VAR, f"elseif {names['op']}=={ops[OpCode.LOAD_VAR.value]} then {names['push']}({names['loc']}[{names['a']}+1])"),
            (OpCode.STORE_VAR, f"elseif {names['op']}=={ops[OpCode.STORE_VAR.value]} then {names['loc']}[{names['a']}+1]={names['pop']}()"),
            (OpCode.LOAD_GLOBAL, f"elseif {names['op']}=={ops[OpCode.LOAD_GLOBAL.value]} then {names['push']}({names['env']}[{names['const']}[{names['a']}+1]])"),
            (OpCode.STORE_GLOBAL, f"elseif {names['op']}=={ops[OpCode.STORE_GLOBAL.value]} then {names['env']}[{names['const']}[{names['a']}+1]]={names['pop']}()"),
            (OpCode.LOAD_UPVAL, f"elseif {names['op']}=={ops[OpCode.LOAD_UPVAL.value]} then {names['push']}({names['up']}[{names['a']}+1])"),
            (OpCode.STORE_UPVAL, f"elseif {names['op']}=={ops[OpCode.STORE_UPVAL.value]} then {names['up']}[{names['a']}+1]={names['pop']}()"),
            (OpCode.NEW_TABLE, f"elseif {names['op']}=={ops[OpCode.NEW_TABLE.value]} then {names['push']}({{}})"),
            (OpCode.GET_TABLE, f"elseif {names['op']}=={ops[OpCode.GET_TABLE.value]} then local {names['key']}={names['pop']}();{names['push']}({names['pop']}()[{names['key']}])"),
            (OpCode.SET_TABLE, f"elseif {names['op']}=={ops[OpCode.SET_TABLE.value]} then local {names['val']}={names['pop']}();local {names['key']}={names['pop']}();{names['pop']}()[{names['key']}]={names['val']}"),
            (OpCode.GET_FIELD, f"elseif {names['op']}=={ops[OpCode.GET_FIELD.value]} then {names['push']}({names['pop']}()[{names['const']}[{names['a']}+1]])"),
            (OpCode.SET_FIELD, f"elseif {names['op']}=={ops[OpCode.SET_FIELD.value]} then local {names['val']}={names['pop']}();{names['pop']}()[{names['const']}[{names['a']}+1]]={names['val']}"),
            (OpCode.ADD, f"elseif {names['op']}=={ops[OpCode.ADD.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()+{names['b']})"),
            (OpCode.SUB, f"elseif {names['op']}=={ops[OpCode.SUB.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()-{names['b']})"),
            (OpCode.MUL, f"elseif {names['op']}=={ops[OpCode.MUL.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()*{names['b']})"),
            (OpCode.DIV, f"elseif {names['op']}=={ops[OpCode.DIV.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()/{names['b']})"),
            (OpCode.MOD, f"elseif {names['op']}=={ops[OpCode.MOD.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()%{names['b']})"),
            (OpCode.POW, f"elseif {names['op']}=={ops[OpCode.POW.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()^{names['b']})"),
            (OpCode.UNM, f"elseif {names['op']}=={ops[OpCode.UNM.value]} then {names['push']}(-{names['pop']}())"),
            (OpCode.CONCAT, f"elseif {names['op']}=={ops[OpCode.CONCAT.value]} then local {names['b']}=_ts({names['pop']}());{names['push']}(_ts({names['pop']}())..{names['b']})"),
            (OpCode.LEN, f"elseif {names['op']}=={ops[OpCode.LEN.value]} then {names['push']}(#{names['pop']}())"),
            (OpCode.EQ, f"elseif {names['op']}=={ops[OpCode.EQ.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()=={names['b']})"),
            (OpCode.NE, f"elseif {names['op']}=={ops[OpCode.NE.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()~={names['b']})"),
            (OpCode.LT, f"elseif {names['op']}=={ops[OpCode.LT.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()<{names['b']})"),
            (OpCode.LE, f"elseif {names['op']}=={ops[OpCode.LE.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()<={names['b']})"),
            (OpCode.GT, f"elseif {names['op']}=={ops[OpCode.GT.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()>{names['b']})"),
            (OpCode.GE, f"elseif {names['op']}=={ops[OpCode.GE.value]} then local {names['b']}={names['pop']}();{names['push']}({names['pop']}()>={names['b']})"),
            (OpCode.NOT, f"elseif {names['op']}=={ops[OpCode.NOT.value]} then {names['push']}(not {names['pop']}())"),
            (OpCode.JMP, f"elseif {names['op']}=={ops[OpCode.JMP.value]} then {names['pc']}={names['a']}+1"),
            (OpCode.JMP_IF, f"elseif {names['op']}=={ops[OpCode.JMP_IF.value]} then if {names['peek']}() then {names['pc']}={names['a']}+1 end"),
            (OpCode.JMP_IF_NOT, f"elseif {names['op']}=={ops[OpCode.JMP_IF_NOT.value]} then if not {names['pop']}() then {names['pc']}={names['a']}+1 end"),
            (OpCode.CALL, f"elseif {names['op']}=={ops[OpCode.CALL.value]} then local {names['args']}={{}};for {names['i']}={names['a']},1,-1 do {names['args']}[{names['i']}]={names['pop']}() end;local {names['res']}={{{names['pop']}()(_u({names['args']}))}};for _,{names['val']} in _i({names['res']}) do {names['push']}({names['val']}) end"),
            (OpCode.SELF, f"elseif {names['op']}=={ops[OpCode.SELF.value]} then local {names['args']}={{}};for {names['i']}={names['a']},1,-1 do {names['args']}[{names['i']}]={names['pop']}() end;local {names['tmp']}={names['pop']}();local {names['obj']}={names['pop']}();local {names['res']}={{{names['tmp']}({names['obj']},_u({names['args']}))}};for _,{names['val']} in _i({names['res']}) do {names['push']}({names['val']}) end"),
            (OpCode.RETURN, f"elseif {names['op']}=={ops[OpCode.RETURN.value]} then local {names['res']}={{}};for {names['i']}={names['a']},1,-1 do {names['res']}[{names['i']}]={names['pop']}() end;return _u({names['res']})"),
            (OpCode.CLOSURE, f"elseif {names['op']}=={ops[OpCode.CLOSURE.value]} then {names['push']}({names['create']}({names['child']}[{names['a']}+1],{names['env']},{names['loc']}))"),
            (OpCode.VARARG, f"elseif {names['op']}=={ops[OpCode.VARARG.value]} then for {names['i']}={names['np']}+1,{names['nv']} do {names['push']}({names['varg']}[{names['i']}]) end"),
            (OpCode.DUP, f"elseif {names['op']}=={ops[OpCode.DUP.value]} then {names['push']}({names['peek']}())"),
            (OpCode.POP, f"elseif {names['op']}=={ops[OpCode.POP.value]} then {names['pop']}()"),
            (OpCode.FORPREP, f"elseif {names['op']}=={ops[OpCode.FORPREP.value]} then local {names['step']}={names['pop']}();local {names['limit']}={names['pop']}();local {names['init']}={names['pop']}();{names['loc']}[{names['a']}+1]={names['init']}-{names['step']};{names['push']}({names['init']});{names['push']}({names['limit']});{names['push']}({names['step']})"),
            (OpCode.FORLOOP, f"elseif {names['op']}=={ops[OpCode.FORLOOP.value]} then local {names['step']}={names['peek']}(0);local {names['limit']}={names['peek']}(1);local {names['idx']}={names['loc']}[{names['a']}+1]+{names['step']};{names['loc']}[{names['a']}+1]={names['idx']};if({names['step']}>0 and {names['idx']}<={names['limit']})or({names['step']}<=0 and {names['idx']}>={names['limit']})then {names['pc']}={names['a']} else {names['pop']}();{names['pop']}();{names['pop']}() end"),
            (OpCode.TFORLOOP, f"elseif {names['op']}=={ops[OpCode.TFORLOOP.value]} then local {names['iter']}={names['peek']}(2);local {names['state']}={names['peek']}(1);local {names['ctrl']}={names['peek']}(0);local {names['res']}={{{names['iter']}({names['state']},{names['ctrl']})}};if {names['res']}[1]==nil then {names['pc']}={names['a']}+1 else for {names['i']}=1,{names['a']} do {names['loc']}[{names['i']}]={names['res']}[{names['i']}] end;{names['stk']}[{names['top']}]={names['res']}[1] end"),
        ]
        
        for _, handler in handlers:
            vm_parts.append(handler)
        
        vm_parts.append("end end end;")
        vm_parts.append(f"return {names['exec']};end;")
        vm_parts.append(f"return {names['create']};")
        
        return ''.join(vm_parts)
    
    def _generate_junk_data(self, size: int) -> str:
        """Generate junk data to increase size"""
        junk_parts = []
        
        # Random data tables
        for _ in range(size // 100):
            data = ','.join(str(random.randint(0, 255)) for _ in range(random.randint(20, 50)))
            name = self._rand_name()
            junk_parts.append(f"local {name}={{{data}}};")
        
        # Fake function tables
        for _ in range(size // 200):
            name = self._rand_name()
            inner = ','.join(f'[{self._encode_num(random.randint(0, 100))}]={self._encode_num(random.randint(0, 9999))}' for _ in range(random.randint(5, 15)))
            junk_parts.append(f"local {name}={{{inner}}};")
        
        # Complex expressions
        for _ in range(size // 300):
            name = self._rand_name()
            expr = '+'.join(str(random.randint(1, 999)) for _ in range(random.randint(3, 8)))
            junk_parts.append(f"local {name}=({expr});")
        
        return ''.join(junk_parts)
    
    def _generate_anti_analysis(self) -> str:
        """Generate anti-analysis code"""
        v = [self._rand_name() for _ in range(10)]
        
        code = f"local {v[0]}=pcall;local {v[1]}=type;local {v[2]}=error;local {v[3]}=pairs;local {v[4]}=tostring;local {v[5]}=getmetatable;local {v[6]}=setmetatable;local {v[7]}=rawget;local {v[8]}=rawset;local {v[9]}=select;"
        code += f"(function()local {self._rand_name()}={{[1]={v[0]},[2]={v[1]},[3]={v[3]}}};for {self._rand_name()},{self._rand_name()} in {v[3]}({self._rand_name()}) do if {v[1]}({self._rand_name()})~='function' then return end end end)();"
        
        return code
    
    def _generate_fake_complexity(self) -> str:
        """Generate fake complex code structures"""
        parts = []
        
        # Fake coroutine handling
        v = [self._rand_name() for _ in range(6)]
        parts.append(f"local {v[0]}={{d=coroutine.yield,e=coroutine.wrap,f=coroutine.create,g=coroutine.resume,h=coroutine.status,i=coroutine.running}};")
        
        # Fake bit operations
        parts.append(f"local {v[1]}=bit32 or bit or{{}};local {v[2]}={v[1]}.band or function(a,b)return a end;local {v[3]}={v[1]}.bor or function(a,b)return b end;local {v[4]}={v[1]}.bxor or function(a,b)return a~b end;local {v[5]}={v[1]}.bnot or function(a)return~a end;")
        
        # Fake string operations
        n = [self._rand_name() for _ in range(4)]
        parts.append(f"local {n[0]}=string;local {n[1]}={n[0]}.byte;local {n[2]}={n[0]}.char;local {n[3]}={n[0]}.sub;")
        
        # Fake math operations
        m = [self._rand_name() for _ in range(5)]
        parts.append(f"local {m[0]}=math;local {m[1]}={m[0]}.floor;local {m[2]}={m[0]}.ceil;local {m[3]}={m[0]}.abs;local {m[4]}={m[0]}.random;")
        
        # Fake tables
        for _ in range(3):
            t = self._rand_name()
            vals = ','.join(f'{self._rand_name()}={random.randint(0,999)}' for _ in range(random.randint(3, 8)))
            parts.append(f"local {t}={{{vals}}};")
        
        return ''.join(parts)
    
    def generate(self, func: CompiledFunction, mode: str = "loadstring") -> str:
        """Generate dense, professional obfuscated code"""
        # Calculate target size (600-800% ratio)
        base_size = len(str(func.constants)) + len(func.instructions) * 10
        target_junk_size = base_size * 3  # Add junk to reach target ratio
        
        # Header
        header = f"--[[ LuaShield Pro v2.1 | luashield.dev | {hashlib.md5(str(random.random()).encode()).hexdigest()[:16]} ]]"
        
        # Anti-analysis layer
        anti = self._generate_anti_analysis()
        
        # Fake complexity
        fake = self._generate_fake_complexity()
        
        # Junk data
        junk = self._generate_junk_data(target_junk_size)
        
        # VM runtime
        vm = self._generate_dense_vm()
        
        # Bytecode
        bytecode = self._serialize_function(func)
        
        # Main execution
        main_var = self._rand_name()
        bc_var = self._rand_name()
        vm_var = self._rand_name()
        
        main = f"local {vm_var}=(function(){vm}end)();local {bc_var}={bytecode};local {main_var}={vm_var}({bc_var});return {main_var}(...);"
        
        # Combine all parts
        full_code = header + anti + fake + junk + main
        
        # Make it dense - fill lines to ~200 chars
        dense_code = self._make_dense(full_code, 180)
        
        return dense_code
