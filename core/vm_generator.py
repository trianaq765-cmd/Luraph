import random
import string
import hashlib
from typing import List, Dict, Any, Tuple
from .compiler import CompiledFunction, OpCode, Instruction

class VMGenerator:
    """
    Ultra Dense VM Generator - Luraph/Moonsec Style
    - Setiap baris PENUH 200+ karakter
    - TIDAK ADA spasi/newline yang tidak perlu
    - Ratio 600-800%
    """
    
    def __init__(self, key: bytes = None, use_encryption: bool = True):
        self.key = key or bytes([random.randint(0, 255) for _ in range(32)])
        self.use_encryption = use_encryption
        self.opcode_map: Dict[int, int] = {}
        self.generated_names: set = set()
        self.name_counter = 0
        self._generate_opcode_mapping()
    
    def _generate_opcode_mapping(self):
        available = list(range(1, 200))
        random.shuffle(available)
        for op in OpCode:
            if available:
                self.opcode_map[op.value] = available.pop()
    
    def _rn(self, min_len: int = 2, max_len: int = 6) -> str:
        """Generate random name - short but unique"""
        self.name_counter += 1
        length = random.randint(min_len, max_len)
        
        # Mix different styles
        style = self.name_counter % 4
        if style == 0:
            chars = ''.join(random.choice('lIi1O0oQCKSZXVWY') for _ in range(length))
        elif style == 1:
            chars = '_0x' + ''.join(random.choice('0123456789abcdef') for _ in range(length))
        elif style == 2:
            chars = '_' + ''.join(random.choice('abcdefghijklmnop') for _ in range(length))
        else:
            chars = ''.join(random.choice('ABCDEFGHIJKLMNOP') for _ in range(length))
        
        name = chars + str(self.name_counter)
        while name in self.generated_names or name[0].isdigit():
            name = '_' + name
        self.generated_names.add(name)
        return name
    
    def _en(self, n: int) -> str:
        """Encode number - various formats"""
        if n < 0:
            return str(n)
        if n == 0:
            return random.choice(['0', '0x0', '(0)'])
        
        r = random.random()
        if r < 0.3 and n < 256:
            return f'0x{n:X}'
        elif r < 0.5 and n < 128:
            return f'0B{bin(n)[2:]}'
        elif r < 0.7:
            return f'(0x{n:X})' if n < 4096 else str(n)
        return str(n)
    
    def _es(self, s: str) -> str:
        """Encode string dengan XOR"""
        if not s:
            return '""'
        key = [random.randint(1, 254) for _ in range(random.randint(4, 12))]
        enc = [ord(c) ^ key[i % len(key)] for i, c in enumerate(s)]
        return f'_D({{{",".join(map(str,key))}}},{{{",".join(map(str,enc))}}})'
    
    def _pack_dense(self, code: str, line_len: int = 220) -> str:
        """Pack code into dense lines - NO SPARSE LINES"""
        # Remove all whitespace first
        code = code.replace('\n', '').replace('\r', '')
        # Collapse multiple spaces
        while '  ' in code:
            code = code.replace('  ', ' ')
        # Remove spaces around operators
        for op in ['=', '+', '-', '*', '/', '%', '^', '<', '>', '~', ',', ';', '(', ')', '{', '}', '[', ']']:
            code = code.replace(f' {op} ', op).replace(f' {op}', op).replace(f'{op} ', op)
        # Add back necessary spaces
        code = code.replace('local', ' local ').replace('function', ' function ')
        code = code.replace('then', ' then ').replace('else', ' else ')
        code = code.replace('elseif', ' elseif ').replace('end', ' end ')
        code = code.replace('do', ' do ').replace('for', ' for ')
        code = code.replace('while', ' while ').replace('repeat', ' repeat ')
        code = code.replace('until', ' until ').replace('return', ' return ')
        code = code.replace('and', ' and ').replace('or', ' or ')
        code = code.replace('not', ' not ').replace('in', ' in ')
        # Clean up
        while '  ' in code:
            code = code.replace('  ', ' ')
        code = code.strip()
        
        # Split into lines of exactly line_len chars
        lines = []
        while len(code) > line_len:
            # Find safe break point
            break_point = line_len
            # Try to break at semicolon or space
            for i in range(line_len, max(line_len - 50, 0), -1):
                if code[i] in ';, ':
                    break_point = i + 1
                    break
            lines.append(code[:break_point])
            code = code[break_point:]
        if code:
            lines.append(code)
        
        return '\n'.join(lines)
    
    def _gen_const(self, constants: List[Any]) -> str:
        """Serialize constants"""
        parts = []
        for c in constants:
            if c is None:
                parts.append('nil')
            elif isinstance(c, bool):
                parts.append('true' if c else 'false')
            elif isinstance(c, int):
                parts.append(self._en(c))
            elif isinstance(c, float):
                if c == float('inf'):
                    parts.append('(1/0)')
                elif c == float('-inf'):
                    parts.append('(-1/0)')
                elif c != c:
                    parts.append('(0/0)')
                else:
                    parts.append(str(c))
            elif isinstance(c, str):
                parts.append(self._es(c))
            else:
                parts.append('nil')
        return '{' + ','.join(parts) + '}'
    
    def _gen_instr(self, instructions: List[Instruction]) -> str:
        """Serialize instructions"""
        parts = []
        for instr in instructions:
            op = self.opcode_map.get(instr.opcode.value, instr.opcode.value)
            if instr.operand is not None:
                parts.append(f'{{{self._en(op)},{self._en(instr.operand)}}}')
            else:
                parts.append(f'{{{self._en(op)}}}')
        return '{' + ','.join(parts) + '}'
    
    def _gen_func(self, func: CompiledFunction) -> str:
        """Serialize function"""
        children = '{}'
        if func.children:
            children = '{' + ','.join(self._gen_func(c) for c in func.children) + '}'
        return f'{{{self._gen_const(func.constants)},{self._gen_instr(func.instructions)},{len(func.params)},{1 if func.is_vararg else 0},{children}}}'
    
    def _gen_junk_tables(self, count: int) -> str:
        """Generate junk data tables"""
        parts = []
        for _ in range(count):
            name = self._rn()
            data = ','.join(str(random.randint(0, 255)) for _ in range(random.randint(30, 80)))
            parts.append(f'local {name}={{{data}}};')
        return ''.join(parts)
    
    def _gen_junk_funcs(self, count: int) -> str:
        """Generate junk function-like structures"""
        parts = []
        for _ in range(count):
            name = self._rn()
            keys = ','.join(f'{self._rn()}={random.randint(0,999)}' for _ in range(random.randint(5, 15)))
            parts.append(f'local {name}={{{keys}}};')
        return ''.join(parts)
    
    def _gen_junk_exprs(self, count: int) -> str:
        """Generate junk expressions"""
        parts = []
        for _ in range(count):
            name = self._rn()
            nums = '+'.join(str(random.randint(1, 999)) for _ in range(random.randint(3, 8)))
            parts.append(f'local {name}=({nums});')
        return ''.join(parts)
    
    def _gen_fake_refs(self) -> str:
        """Generate fake library references"""
        refs = []
        # Coroutine
        n = [self._rn() for _ in range(6)]
        refs.append(f'local {n[0]}={{d=coroutine.yield,e=coroutine.wrap,f=coroutine.create,g=coroutine.resume,h=coroutine.status,i=coroutine.running}};')
        # Bit32
        refs.append(f'local {n[1]}=bit32 or bit or{{}};local {n[2]}={n[1]}.band or function(a,b)return a end;local {n[3]}={n[1]}.bor or function(a,b)return b end;local {n[4]}={n[1]}.bxor or function(a,b)return a~b end;local {n[5]}={n[1]}.bnot or function(a)return~a end;')
        # String
        m = [self._rn() for _ in range(5)]
        refs.append(f'local {m[0]}=string;local {m[1]}={m[0]}.byte;local {m[2]}={m[0]}.char;local {m[3]}={m[0]}.sub;local {m[4]}={m[0]}.gsub;')
        # Math
        refs.append(f'local {self._rn()}=math;local {self._rn()}=math.floor;local {self._rn()}=math.ceil;local {self._rn()}=math.abs;local {self._rn()}=math.random;')
        # Table
        refs.append(f'local {self._rn()}=table;local {self._rn()}=table.insert;local {self._rn()}=table.remove;local {self._rn()}=table.concat;')
        return ''.join(refs)
    
    def _gen_anti(self) -> str:
        """Generate anti-tampering"""
        v = [self._rn() for _ in range(8)]
        return f'local {v[0]}=pcall;local {v[1]}=type;local {v[2]}=pairs;local {v[3]}=error;local {v[4]}=tostring;local {v[5]}=getmetatable;local {v[6]}=setmetatable;local {v[7]}=select;(function()local {self._rn()}={{[1]={v[0]},[2]={v[1]},[3]={v[2]}}};for {self._rn()},{self._rn()} in {v[2]}({self._rn()})do if {v[1]}({self._rn()})~="function"then return end end end)();'
    
    def _gen_vm(self) -> str:
        """Generate compact but complete VM"""
        # All variable names
        v = {k: self._rn(2, 4) for k in [
            'D', 'M', 'X', 'K', 'C', 'P', 'F', 'V', 'N', 'S', 'T', 'I', 'L',
            'H', 'G', 'W', 'O', 'A', 'B', 'R', 'E', 'U', 'J', 'Q', 'Y', 'Z'
        ]}
        
        ops = self.opcode_map
        
        # Build VM as continuous string
        vm = f'local {v["D"]}=function({v["K"]},{v["V"]})local {v["R"]}={{}};for {v["I"]}=1,#{v["V"]} do {v["R"]}[{v["I"]}]=string.char(bit32 and bit32.bxor({v["V"]}[{v["I"]}],{v["K"]}[({v["I"]}-1)%#{v["K"]}+1])or({v["V"]}[{v["I"]}]~{v["K"]}[({v["I"]}-1)%#{v["K"]}+1]))end;return table.concat({v["R"]})end;'
        vm += f'local _D={v["D"]};local _u=unpack or table.unpack;local _s=select;local _p=pairs;local _i=ipairs;'
        vm += f'local {v["M"]};{v["M"]}=function({v["F"]},{v["E"]},{v["U"]}){v["E"]}={v["E"]}or(getfenv and getfenv(0))or _ENV or _G;'
        vm += f'local {v["X"]};{v["X"]}=function(...)local {v["K"]}={v["F"]}[1];local {v["C"]}={v["F"]}[2];local {v["P"]}={v["F"]}[3];local {v["J"]}={v["F"]}[5];'
        vm += f'local {v["V"]}={{...}};local {v["N"]}=_s("#",...);local {v["S"]}={{}};local {v["T"]}=0;local {v["I"]}=1;local {v["L"]}={{}};'
        vm += f'for {v["Q"]}=1,{v["P"]} do {v["L"]}[{v["Q"]}]={v["V"]}[{v["Q"]}]end;'
        vm += f'local function {v["H"]}({v["Z"]}){v["T"]}={v["T"]}+1;{v["S"]}[{v["T"]}]={v["Z"]};end;'
        vm += f'local function {v["G"]}()local {v["Z"]}={v["S"]}[{v["T"]}];{v["S"]}[{v["T"]}]=nil;{v["T"]}={v["T"]}-1;return {v["Z"]};end;'
        vm += f'local function {v["W"]}({v["Y"]})return {v["S"]}[{v["T"]}-({v["Y"]}or 0)];end;'
        vm += f'while {v["I"]}<=#({v["C"]})do local {v["O"]}={v["C"]}[{v["I"]}][1];local {v["A"]}={v["C"]}[{v["I"]}][2];{v["I"]}={v["I"]}+1;'
        
        # All opcode handlers in one continuous line
        handlers = [
            (OpCode.LOAD_CONST, f'if {v["O"]}=={ops[OpCode.LOAD_CONST.value]}then {v["H"]}({v["K"]}[{v["A"]}+1])'),
            (OpCode.LOAD_NIL, f'elseif {v["O"]}=={ops[OpCode.LOAD_NIL.value]}then {v["H"]}(nil)'),
            (OpCode.LOAD_TRUE, f'elseif {v["O"]}=={ops[OpCode.LOAD_TRUE.value]}then {v["H"]}(true)'),
            (OpCode.LOAD_FALSE, f'elseif {v["O"]}=={ops[OpCode.LOAD_FALSE.value]}then {v["H"]}(false)'),
            (OpCode.LOAD_VAR, f'elseif {v["O"]}=={ops[OpCode.LOAD_VAR.value]}then {v["H"]}({v["L"]}[{v["A"]}+1])'),
            (OpCode.STORE_VAR, f'elseif {v["O"]}=={ops[OpCode.STORE_VAR.value]}then {v["L"]}[{v["A"]}+1]={v["G"]}()'),
            (OpCode.LOAD_GLOBAL, f'elseif {v["O"]}=={ops[OpCode.LOAD_GLOBAL.value]}then {v["H"]}({v["E"]}[{v["K"]}[{v["A"]}+1]])'),
            (OpCode.STORE_GLOBAL, f'elseif {v["O"]}=={ops[OpCode.STORE_GLOBAL.value]}then {v["E"]}[{v["K"]}[{v["A"]}+1]]={v["G"]}()'),
            (OpCode.LOAD_UPVAL, f'elseif {v["O"]}=={ops[OpCode.LOAD_UPVAL.value]}then {v["H"]}({v["U"]}[{v["A"]}+1])'),
            (OpCode.STORE_UPVAL, f'elseif {v["O"]}=={ops[OpCode.STORE_UPVAL.value]}then {v["U"]}[{v["A"]}+1]={v["G"]}()'),
            (OpCode.NEW_TABLE, f'elseif {v["O"]}=={ops[OpCode.NEW_TABLE.value]}then {v["H"]}({{}})'),
            (OpCode.GET_TABLE, f'elseif {v["O"]}=={ops[OpCode.GET_TABLE.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()[{v["B"]}])'),
            (OpCode.SET_TABLE, f'elseif {v["O"]}=={ops[OpCode.SET_TABLE.value]}then local {v["Z"]}={v["G"]}();local {v["B"]}={v["G"]}();{v["G"]}()[{v["B"]}]={v["Z"]}'),
            (OpCode.GET_FIELD, f'elseif {v["O"]}=={ops[OpCode.GET_FIELD.value]}then {v["H"]}({v["G"]}()[{v["K"]}[{v["A"]}+1]])'),
            (OpCode.SET_FIELD, f'elseif {v["O"]}=={ops[OpCode.SET_FIELD.value]}then local {v["Z"]}={v["G"]}();{v["G"]}()[{v["K"]}[{v["A"]}+1]]={v["Z"]}'),
            (OpCode.ADD, f'elseif {v["O"]}=={ops[OpCode.ADD.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()+{v["B"]})'),
            (OpCode.SUB, f'elseif {v["O"]}=={ops[OpCode.SUB.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()-{v["B"]})'),
            (OpCode.MUL, f'elseif {v["O"]}=={ops[OpCode.MUL.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()*{v["B"]})'),
            (OpCode.DIV, f'elseif {v["O"]}=={ops[OpCode.DIV.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()/{v["B"]})'),
            (OpCode.MOD, f'elseif {v["O"]}=={ops[OpCode.MOD.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()%{v["B"]})'),
            (OpCode.POW, f'elseif {v["O"]}=={ops[OpCode.POW.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()^{v["B"]})'),
            (OpCode.UNM, f'elseif {v["O"]}=={ops[OpCode.UNM.value]}then {v["H"]}(-{v["G"]}())'),
            (OpCode.CONCAT, f'elseif {v["O"]}=={ops[OpCode.CONCAT.value]}then local {v["B"]}=tostring({v["G"]}());{v["H"]}(tostring({v["G"]}())..{v["B"]})'),
            (OpCode.LEN, f'elseif {v["O"]}=={ops[OpCode.LEN.value]}then {v["H"]}(#{v["G"]}())'),
            (OpCode.EQ, f'elseif {v["O"]}=={ops[OpCode.EQ.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()=={v["B"]})'),
            (OpCode.NE, f'elseif {v["O"]}=={ops[OpCode.NE.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()~={v["B"]})'),
            (OpCode.LT, f'elseif {v["O"]}=={ops[OpCode.LT.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()<{v["B"]})'),
            (OpCode.LE, f'elseif {v["O"]}=={ops[OpCode.LE.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()<={v["B"]})'),
            (OpCode.GT, f'elseif {v["O"]}=={ops[OpCode.GT.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()>{v["B"]})'),
            (OpCode.GE, f'elseif {v["O"]}=={ops[OpCode.GE.value]}then local {v["B"]}={v["G"]}();{v["H"]}({v["G"]}()>={v["B"]})'),
            (OpCode.NOT, f'elseif {v["O"]}=={ops[OpCode.NOT.value]}then {v["H"]}(not {v["G"]}())'),
            (OpCode.JMP, f'elseif {v["O"]}=={ops[OpCode.JMP.value]}then {v["I"]}={v["A"]}+1'),
            (OpCode.JMP_IF, f'elseif {v["O"]}=={ops[OpCode.JMP_IF.value]}then if {v["W"]}()then {v["I"]}={v["A"]}+1 end'),
            (OpCode.JMP_IF_NOT, f'elseif {v["O"]}=={ops[OpCode.JMP_IF_NOT.value]}then if not {v["G"]}()then {v["I"]}={v["A"]}+1 end'),
            (OpCode.CALL, f'elseif {v["O"]}=={ops[OpCode.CALL.value]}then local {v["R"]}={{}};for {v["Q"]}={v["A"]},1,-1 do {v["R"]}[{v["Q"]}]={v["G"]}()end;local {v["Z"]}={{{v["G"]}()(_u({v["R"]}))}};for _,{v["Y"]} in _i({v["Z"]})do {v["H"]}({v["Y"]})end'),
            (OpCode.SELF, f'elseif {v["O"]}=={ops[OpCode.SELF.value]}then local {v["R"]}={{}};for {v["Q"]}={v["A"]},1,-1 do {v["R"]}[{v["Q"]}]={v["G"]}()end;local {v["B"]}={v["G"]}();local {v["Y"]}={v["G"]}();local {v["Z"]}={{{v["B"]}({v["Y"]},_u({v["R"]}))}};for _,{v["Q"]} in _i({v["Z"]})do {v["H"]}({v["Q"]})end'),
            (OpCode.RETURN, f'elseif {v["O"]}=={ops[OpCode.RETURN.value]}then local {v["R"]}={{}};for {v["Q"]}={v["A"]},1,-1 do {v["R"]}[{v["Q"]}]={v["G"]}()end;return _u({v["R"]})'),
            (OpCode.CLOSURE, f'elseif {v["O"]}=={ops[OpCode.CLOSURE.value]}then {v["H"]}({v["M"]}({v["J"]}[{v["A"]}+1],{v["E"]},{v["L"]}))'),
            (OpCode.VARARG, f'elseif {v["O"]}=={ops[OpCode.VARARG.value]}then for {v["Q"]}={v["P"]}+1,{v["N"]} do {v["H"]}({v["V"]}[{v["Q"]}])end'),
            (OpCode.DUP, f'elseif {v["O"]}=={ops[OpCode.DUP.value]}then {v["H"]}({v["W"]}())'),
            (OpCode.POP, f'elseif {v["O"]}=={ops[OpCode.POP.value]}then {v["G"]}()'),
            (OpCode.FORPREP, f'elseif {v["O"]}=={ops[OpCode.FORPREP.value]}then local {v["Z"]}={v["G"]}();local {v["B"]}={v["G"]}();local {v["Y"]}={v["G"]}();{v["L"]}[{v["A"]}+1]={v["Y"]}-{v["Z"]};{v["H"]}({v["Y"]});{v["H"]}({v["B"]});{v["H"]}({v["Z"]})'),
            (OpCode.FORLOOP, f'elseif {v["O"]}=={ops[OpCode.FORLOOP.value]}then local {v["Z"]}={v["W"]}(0);local {v["B"]}={v["W"]}(1);local {v["Y"]}={v["L"]}[{v["A"]}+1]+{v["Z"]};{v["L"]}[{v["A"]}+1]={v["Y"]};if({v["Z"]}>0 and {v["Y"]}<={v["B"]})or({v["Z"]}<=0 and {v["Y"]}>={v["B"]})then {v["I"]}={v["A"]} else {v["G"]}();{v["G"]}();{v["G"]}()end'),
            (OpCode.TFORLOOP, f'elseif {v["O"]}=={ops[OpCode.TFORLOOP.value]}then local {v["Z"]}={v["W"]}(2);local {v["B"]}={v["W"]}(1);local {v["Y"]}={v["W"]}(0);local {v["R"]}={{{v["Z"]}({v["B"]},{v["Y"]})}};if {v["R"]}[1]==nil then {v["I"]}={v["A"]}+1 else for {v["Q"]}=1,{v["A"]} do {v["L"]}[{v["Q"]}]={v["R"]}[{v["Q"]}]end;{v["S"]}[{v["T"]}]={v["R"]}[1]end'),
        ]
        
        for _, h in handlers:
            vm += h
        
        vm += f'end end end;return {v["X"]};end;return {v["M"]};'
        return vm
    
    def generate(self, func: CompiledFunction, mode: str = "loadstring") -> str:
        """Generate ultra-dense obfuscated code"""
        # Header
        h = hashlib.md5(str(random.random()).encode()).hexdigest()[:16]
        header = f'--[[LuaShield.Pro" v2.1|{h}]]'
        
        # Anti-tampering
        anti = self._gen_anti()
        
        # Fake library refs
        fake = self._gen_fake_refs()
        
        # Calculate junk amount based on original size
        base = len(str(func.constants)) + len(func.instructions) * 5
        junk_count = max(20, base // 50)
        
        # Generate junk
        junk1 = self._gen_junk_tables(junk_count)
        junk2 = self._gen_junk_funcs(junk_count // 2)
        junk3 = self._gen_junk_exprs(junk_count // 3)
        
        # VM
        vm = self._gen_vm()
        
        # Bytecode
        bc = self._gen_func(func)
        
        # Main execution
        vm_var = self._rn()
        bc_var = self._rn()
        main_var = self._rn()
        
        main = f'local {vm_var}=(function(){vm}end)();local {bc_var}={bc};local {main_var}={vm_var}({bc_var});return {main_var}(...);'
        
        # Combine everything
        full = header + anti + fake + junk1 + junk2 + junk3 + main
        
        # Pack into dense lines (200+ chars per line)
        return self._pack_dense(full, 220)
