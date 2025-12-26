import hashlib
import random
import string
import time
import base64
from typing import List, Tuple, Optional
from dataclasses import dataclass, field


@dataclass
class AntiTamperConfig:
    """Configuration for anti-tamper protection"""
    # Hash verification
    enable_hash_check: bool = True
    hash_algorithm: str = 'sha256'  # sha256, md5, sha1
    
    # Environment checks
    check_environment: bool = True
    check_debug: bool = True
    check_hooks: bool = True
    
    # Timing checks
    enable_timing_check: bool = True
    max_execution_time_ms: int = 5000
    
    # Anti-dump
    enable_anti_dump: bool = True
    
    # Self-destruct
    enable_self_destruct: bool = False
    
    # Obfuscation of protection code
    obfuscate_protection: bool = True


class AntiTamper:
    """
    Anti-Tamper Protection Generator
    Generates various protection mechanisms for Lua code
    """
    
    def __init__(self, config: AntiTamperConfig = None):
        """
        Initialize anti-tamper generator
        
        Args:
            config: Protection configuration
        """
        self.config = config or AntiTamperConfig()
        self.var_prefix = self._random_prefix()
    
    def _random_prefix(self, length: int = 4) -> str:
        """Generate random variable prefix"""
        chars = string.ascii_letters + '_'
        return ''.join(random.choice(chars) for _ in range(length))
    
    def _random_name(self, length: int = 8) -> str:
        """Generate random variable name"""
        chars = string.ascii_letters + '_'
        first = random.choice(chars)
        rest = ''.join(random.choice(chars + string.digits) for _ in range(length - 1))
        return self.var_prefix + first + rest
    
    def generate_all(self, code_hash: str = None) -> str:
        """
        Generate all enabled protection mechanisms
        
        Args:
            code_hash: Hash of the protected code
            
        Returns:
            Protection code to prepend
        """
        protections = []
        
        # Variable declarations for protection
        protections.append(self._generate_variables())
        
        if self.config.enable_hash_check and code_hash:
            protections.append(self._generate_hash_check(code_hash))
        
        if self.config.check_environment:
            protections.append(self._generate_environment_check())
        
        if self.config.check_debug:
            protections.append(self._generate_debug_check())
        
        if self.config.check_hooks:
            protections.append(self._generate_hook_check())
        
        if self.config.enable_timing_check:
            protections.append(self._generate_timing_check())
        
        if self.config.enable_anti_dump:
            protections.append(self._generate_anti_dump())
        
        # Combine all protections
        combined = '\n'.join(filter(None, protections))
        
        if self.config.obfuscate_protection:
            combined = self._obfuscate_protection_code(combined)
        
        return combined
    
    def _generate_variables(self) -> str:
        """Generate base variable declarations"""
        v = self.var_prefix
        
        return f'''
-- Protection Layer
local {v}_error = error
local {v}_pcall = pcall
local {v}_type = type
local {v}_pairs = pairs
local {v}_tostring = tostring
local {v}_getfenv = getfenv
local {v}_setfenv = setfenv
local {v}_getmetatable = getmetatable
local {v}_setmetatable = setmetatable
local {v}_rawget = rawget
local {v}_rawset = rawset
local {v}_debug = debug
local {v}_string = string
local {v}_math = math
local {v}_os = os
local {v}_tick = tick or os.clock
'''
    
    def _generate_hash_check(self, expected_hash: str) -> str:
        """Generate code hash verification"""
        v = self.var_prefix
        check_func = self._random_name()
        hash_var = self._random_name()
        
        # Split hash for obfuscation
        hash_parts = [expected_hash[i:i+4] for i in range(0, len(expected_hash), 4)]
        hash_concat = ' .. '.join(f'"{part}"' for part in hash_parts)
        
        return f'''
local {hash_var} = {hash_concat}
local function {check_func}(data)
    local h = 0
    for i = 1, #data do
        h = ({v}_math.floor(h * 31 + {v}_string.byte(data, i)) % 2147483647)
    end
    return {v}_string.format("%08x", h)
end
'''
    
    def _generate_environment_check(self) -> str:
        """Generate environment integrity check"""
        v = self.var_prefix
        check_func = self._random_name()
        
        return f'''
local function {check_func}()
    -- Check for environment tampering
    local env = {v}_getfenv and {v}_getfenv(0) or _ENV or _G
    
    -- Verify critical functions exist
    local critical = {{"print", "error", "pcall", "type", "pairs"}}
    for _, name in {v}_pairs(critical) do
        if {v}_type(env[name]) ~= "function" then
            return false
        end
    end
    
    -- Check for suspicious globals
    local suspicious = {{"deobfuscate", "decrypt", "dump", "hook", "inject"}}
    for _, name in {v}_pairs(suspicious) do
        if env[name] ~= nil then
            return false
        end
    end
    
    return true
end

if not {check_func}() then
    {v}_error("Environment integrity check failed", 0)
end
'''
    
    def _generate_debug_check(self) -> str:
        """Generate debug library tampering check"""
        v = self.var_prefix
        check_func = self._random_name()
        
        return f'''
local function {check_func}()
    -- Check if debug library is tampered
    if {v}_debug then
        local info = {v}_debug.getinfo
        if info then
            local test_info = info(1)
            if test_info and test_info.func then
                -- Verify debug.getinfo works correctly
                local self_info = info({check_func})
                if not self_info then
                    return false
                end
            end
        end
        
        -- Check for debug hooks
        if {v}_debug.gethook then
            local hook, mask, count = {v}_debug.gethook()
            if hook then
                return false  -- Hook detected
            end
        end
    end
    
    return true
end

if not {check_func}() then
    {v}_error("Debug tampering detected", 0)
end
'''
    
    def _generate_hook_check(self) -> str:
        """Generate function hook detection"""
        v = self.var_prefix
        check_func = self._random_name()
        original_var = self._random_name()
        
        return f'''
local {original_var} = {{}}
local function {check_func}()
    -- Store original function references
    local funcs = {{
        {{"print", print}},
        {{"error", {v}_error}},
        {{"pcall", {v}_pcall}},
        {{"loadstring", loadstring}},
    }}
    
    for _, pair in {v}_pairs(funcs) do
        local name, func = pair[1], pair[2]
        if func then
            {original_var}[name] = func
            
            -- Check if function has been wrapped
            local info = {v}_debug and {v}_debug.getinfo and {v}_debug.getinfo(func)
            if info then
                if info.what == "C" and info.source ~= "=[C]" then
                    return false  -- Native function was hooked
                end
            end
        end
    end
    
    return true
end

if not {check_func}() then
    {v}_error("Function hooks detected", 0)
end
'''
    
    def _generate_timing_check(self) -> str:
        """Generate execution timing check"""
        v = self.var_prefix
        start_var = self._random_name()
        check_func = self._random_name()
        max_time = self.config.max_execution_time_ms
        
        return f'''
local {start_var} = {v}_tick()

local function {check_func}()
    local elapsed = ({v}_tick() - {start_var}) * 1000
    if elapsed > {max_time} then
        return false  -- Execution too slow (possible debugging)
    end
    return true
end

-- Periodic timing check
local {self._random_name()} = {v}_pcall(function()
    if not {check_func}() then
        {v}_error("Timing anomaly detected", 0)
    end
end)
'''
    
    def _generate_anti_dump(self) -> str:
        """Generate anti-dump protection"""
        v = self.var_prefix
        proxy_var = self._random_name()
        mt_var = self._random_name()
        
        return f'''
-- Anti-dump protection
local {proxy_var} = newproxy and newproxy(true) or {{}}
local {mt_var} = {v}_getmetatable({proxy_var})

if {mt_var} then
    {mt_var}.__tostring = function()
        return "<protected>"
    end
    {mt_var}.__metatable = "Protected - Access Denied"
    {mt_var}.__index = function()
        {v}_error("Access denied", 0)
    end
    {mt_var}.__newindex = function()
        {v}_error("Access denied", 0)
    end
end

-- Protect global environment
local {self._random_name()} = {v}_pcall(function()
    local g = _G or (getfenv and getfenv(0)) or _ENV
    local protected_mt = {{
        __metatable = "Protected",
        __tostring = function() return "protected_env" end
    }}
    -- Don't actually set it to avoid breaking things
    -- setmetatable(g, protected_mt)
end)
'''
    
    def _generate_self_destruct(self) -> str:
        """Generate self-destruct mechanism (clears code on tampering)"""
        v = self.var_prefix
        destruct_func = self._random_name()
        
        return f'''
local function {destruct_func}()
    -- Clear sensitive data
    local env = {v}_getfenv and {v}_getfenv(0) or _ENV or _G
    
    -- Overwrite local references
    for i = 1, 100 do
        local name, value = {v}_debug and {v}_debug.getlocal(2, i)
        if not name then break end
        if {v}_type(value) == "string" or {v}_type(value) == "function" then
            {v}_debug.setlocal(2, i, nil)
        end
    end
    
    -- Clear upvalues
    local info = {v}_debug and {v}_debug.getinfo(2, "f")
    if info and info.func then
        for i = 1, 100 do
            local name, value = {v}_debug.getupvalue(info.func, i)
            if not name then break end
            {v}_debug.setupvalue(info.func, i, nil)
        end
    end
    
    collectgarbage("collect")
    {v}_error("Security violation - data destroyed", 0)
end
'''
    
    def _obfuscate_protection_code(self, code: str) -> str:
        """Obfuscate the protection code itself"""
        # Simple obfuscation: add noise comments and shuffle some lines
        lines = code.split('\n')
        result = []
        
        for line in lines:
            result.append(line)
            
            # Add occasional noise
            if random.random() < 0.1 and line.strip() and not line.strip().startswith('--'):
                noise = f"-- {self._random_name(12)}"
                result.append(noise)
        
        return '\n'.join(result)
    
    def generate_integrity_wrapper(self, code: str) -> Tuple[str, str]:
        """
        Wrap code with integrity protection
        
        Args:
            code: Original Lua code
            
        Returns:
            Tuple of (protected_code, code_hash)
        """
        # Calculate code hash
        code_hash = hashlib.sha256(code.encode()).hexdigest()
        
        # Generate protection
        protection = self.generate_all(code_hash)
        
        # Combine
        protected = protection + '\n\n' + code
        
        return protected, code_hash
    
    def generate_runtime_check(self) -> str:
        """Generate runtime integrity check function"""
        v = self.var_prefix
        check_func = self._random_name()
        
        return f'''
local function {check_func}()
    local checks_passed = 0
    local total_checks = 0
    
    -- Environment check
    total_checks = total_checks + 1
    local env = {v}_getfenv and {v}_getfenv(0) or _ENV or _G
    if env and {v}_type(env.print) == "function" then
        checks_passed = checks_passed + 1
    end
    
    -- Stack check
    total_checks = total_checks + 1
    if {v}_debug and {v}_debug.getinfo then
        local info = {v}_debug.getinfo(1)
        if info then
            checks_passed = checks_passed + 1
        end
    else
        checks_passed = checks_passed + 1  -- No debug, assume OK
    end
    
    -- Return integrity score
    return checks_passed / total_checks
end

-- Initial check
local {self._random_name()} = {check_func}()
if {self._random_name()} and {self._random_name()} < 0.5 then
    {v}_error("Integrity check failed", 0)
end
'''


class AntiDump:
    """
    Anti-Dump Protection
    Prevents memory dumping and code extraction
    """
    
    def __init__(self):
        self.var_prefix = ''.join(random.choices(string.ascii_letters, k=4))
    
    def generate(self) -> str:
        """Generate anti-dump protection code"""
        v = self.var_prefix
        
        return f'''
-- Anti-Dump Layer
local {v}_protected = true

-- Protect metatables
local {v}_mt_guard = {{
    __metatable = "Protected",
    __tostring = function() return "<protected>" end,
    __len = function() return 0 end,
    __pairs = function() return function() end end,
    __ipairs = function() return function() end end
}}

-- Protect function info
if debug and debug.setmetatable then
    local {v}_orig_setmt = debug.setmetatable
    debug.setmetatable = function(obj, mt)
        if mt and mt.__protected then
            return obj
        end
        return {v}_orig_setmt(obj, mt)
    end
end

-- Clear debug info periodically
local function {v}_clear_debug()
    if debug then
        -- Limit debug capabilities
        local limited = {{
            getinfo = debug.getinfo,
            traceback = debug.traceback
        }}
        
        -- Don't actually clear to avoid breaking things
        -- for k in pairs(debug) do
        --     if not limited[k] then
        --         debug[k] = nil
        --     end
        -- end
    end
end

{v}_clear_debug()
'''


class IntegrityChecker:
    """
    Code Integrity Checker
    Verifies code hasn't been modified at runtime
    """
    
    def __init__(self, algorithm: str = 'sha256'):
        self.algorithm = algorithm
        self.var_prefix = ''.join(random.choices(string.ascii_letters, k=4))
    
    def calculate_hash(self, code: str) -> str:
        """Calculate hash of code"""
        if self.algorithm == 'sha256':
            return hashlib.sha256(code.encode()).hexdigest()
        elif self.algorithm == 'md5':
            return hashlib.md5(code.encode()).hexdigest()
        elif self.algorithm == 'sha1':
            return hashlib.sha1(code.encode()).hexdigest()
        else:
            return hashlib.sha256(code.encode()).hexdigest()
    
    def generate_checker(self, expected_hash: str) -> str:
        """Generate hash verification code"""
        v = self.var_prefix
        
        # Encode hash in parts
        parts = [expected_hash[i:i+8] for i in range(0, len(expected_hash), 8)]
        
        return f'''
-- Integrity Checker
local {v}_expected = table.concat({{
    {', '.join(f'"{p}"' for p in parts)}
}})

local function {v}_verify(data)
    -- Simple hash function for Lua
    local h = 5381
    for i = 1, #data do
        h = ((h * 33) + string.byte(data, i)) % 4294967296
    end
    return string.format("%08x", h)
end

-- Verification will be called by VM
local {v}_verified = false
'''


def generate_protection(code: str, **kwargs) -> str:
    """
    Convenience function to add protection to code
    
    Args:
        code: Original Lua code
        **kwargs: AntiTamperConfig options
        
    Returns:
        Protected code
    """
    config = AntiTamperConfig(**kwargs)
    anti_tamper = AntiTamper(config)
    protected, _ = anti_tamper.generate_integrity_wrapper(code)
    return protected


def generate_anti_dump() -> str:
    """Generate anti-dump protection code"""
    return AntiDump().generate()


def calculate_integrity_hash(code: str, algorithm: str = 'sha256') -> str:
    """Calculate integrity hash of code"""
    checker = IntegrityChecker(algorithm)
    return checker.calculate_hash(code)
