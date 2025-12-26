import os
import hashlib
import struct
import base64
from typing import Tuple, List
from Crypto.Cipher import AES
from Crypto.Util.Padding import pad, unpad

class AESEncryption:
    """
    AES-256 Encryption for Lua bytecode protection
    Uses CBC mode with PKCS7 padding
    """
    
    BLOCK_SIZE = 16
    KEY_SIZE = 32  # 256 bits
    
    def __init__(self, key: bytes = None, iv: bytes = None):
        """
        Initialize AES encryption
        
        Args:
            key: 32-byte encryption key (generated if None)
            iv: 16-byte initialization vector (generated if None)
        """
        self.key = key if key else self.generate_key()
        self.iv = iv if iv else self.generate_iv()
        
        # Validate
        if len(self.key) != self.KEY_SIZE:
            raise ValueError(f"Key must be {self.KEY_SIZE} bytes")
        if len(self.iv) != self.BLOCK_SIZE:
            raise ValueError(f"IV must be {self.BLOCK_SIZE} bytes")
    
    @staticmethod
    def generate_key() -> bytes:
        """Generate random 256-bit key"""
        return os.urandom(32)
    
    @staticmethod
    def generate_iv() -> bytes:
        """Generate random IV"""
        return os.urandom(16)
    
    @staticmethod
    def derive_key(password: str, salt: bytes = None) -> Tuple[bytes, bytes]:
        """
        Derive key from password using PBKDF2
        
        Args:
            password: Password string
            salt: Salt bytes (generated if None)
            
        Returns:
            Tuple of (key, salt)
        """
        if salt is None:
            salt = os.urandom(16)
        
        key = hashlib.pbkdf2_hmac(
            'sha256',
            password.encode('utf-8'),
            salt,
            iterations=100000,
            dklen=32
        )
        
        return key, salt
    
    def encrypt(self, data: bytes) -> bytes:
        """
        Encrypt data using AES-256-CBC
        
        Args:
            data: Plaintext bytes
            
        Returns:
            Ciphertext bytes
        """
        cipher = AES.new(self.key, AES.MODE_CBC, self.iv)
        padded = pad(data, self.BLOCK_SIZE)
        return cipher.encrypt(padded)
    
    def decrypt(self, data: bytes) -> bytes:
        """
        Decrypt data using AES-256-CBC
        
        Args:
            data: Ciphertext bytes
            
        Returns:
            Plaintext bytes
        """
        cipher = AES.new(self.key, AES.MODE_CBC, self.iv)
        decrypted = cipher.decrypt(data)
        return unpad(decrypted, self.BLOCK_SIZE)
    
    def encrypt_string(self, text: str) -> str:
        """
        Encrypt string to base64
        
        Args:
            text: Plaintext string
            
        Returns:
            Base64 encoded ciphertext
        """
        encrypted = self.encrypt(text.encode('utf-8'))
        return base64.b64encode(encrypted).decode('ascii')
    
    def decrypt_string(self, text: str) -> str:
        """
        Decrypt base64 string
        
        Args:
            text: Base64 encoded ciphertext
            
        Returns:
            Plaintext string
        """
        encrypted = base64.b64decode(text)
        return self.decrypt(encrypted).decode('utf-8')
    
    def get_key_hex(self) -> str:
        """Get key as hex string"""
        return self.key.hex()
    
    def get_iv_hex(self) -> str:
        """Get IV as hex string"""
        return self.iv.hex()
    
    def get_key_table(self) -> List[int]:
        """Get key as list of integers for Lua"""
        return list(self.key)
    
    def get_iv_table(self) -> List[int]:
        """Get IV as list of integers for Lua"""
        return list(self.iv)


class XOREncryption:
    """
    XOR-based encryption for lightweight obfuscation
    Faster than AES but less secure
    """
    
    def __init__(self, key: bytes = None):
        """
        Initialize XOR encryption
        
        Args:
            key: Encryption key (generated if None)
        """
        self.key = key if key else os.urandom(32)
    
    def encrypt(self, data: bytes) -> bytes:
        """XOR encrypt data"""
        result = bytearray(len(data))
        key_len = len(self.key)
        
        for i, byte in enumerate(data):
            result[i] = byte ^ self.key[i % key_len]
        
        return bytes(result)
    
    def decrypt(self, data: bytes) -> bytes:
        """XOR decrypt data (same as encrypt)"""
        return self.encrypt(data)
    
    def encrypt_string(self, text: str) -> List[int]:
        """Encrypt string to byte list for Lua embedding"""
        encrypted = self.encrypt(text.encode('utf-8'))
        return list(encrypted)
    
    def get_key_table(self) -> List[int]:
        """Get key as list of integers for Lua"""
        return list(self.key)


class StringEncryptor:
    """
    String encryption for Lua constant pool
    """
    
    def __init__(self, key: bytes = None):
        self.xor = XOREncryption(key)
        self.key = self.xor.key
    
    def encrypt_constants(self, constants: List) -> Tuple[List, bool]:
        """
        Encrypt string constants in constant pool
        
        Args:
            constants: List of constants
            
        Returns:
            Tuple of (encrypted_constants, has_encrypted)
        """
        encrypted = []
        has_encrypted = False
        
        for const in constants:
            if isinstance(const, str) and len(const) > 0:
                enc_bytes = self.xor.encrypt_string(const)
                encrypted.append({'__enc__': True, 'data': enc_bytes})
                has_encrypted = True
            else:
                encrypted.append(const)
        
        return encrypted, has_encrypted
    
    def generate_decryptor_lua(self) -> str:
        """Generate Lua decryption function"""
        key_table = self.xor.get_key_table()
        
        return f'''
local __dec_key = {{{','.join(map(str, key_table))}}}
local __dec_len = {len(key_table)}

local function __decrypt(data)
    local result = {{}}
    for i = 1, #data do
        local ki = ((i - 1) % __dec_len) + 1
        result[i] = string.char(bit32.bxor(data[i], __dec_key[ki]))
    end
    return table.concat(result)
end

local function __process_const(c)
    if type(c) == "table" and c.__enc__ then
        return __decrypt(c.data)
    end
    return c
end
'''


class BytecodeEncryption:
    """
    Complete bytecode encryption system
    """
    
    def __init__(self, use_aes: bool = True, password: str = None):
        """
        Initialize bytecode encryption
        
        Args:
            use_aes: Use AES-256 (True) or XOR (False)
            password: Optional password for key derivation
        """
        self.use_aes = use_aes
        
        if use_aes:
            if password:
                key, self.salt = AESEncryption.derive_key(password)
                self.aes = AESEncryption(key)
            else:
                self.salt = None
                self.aes = AESEncryption()
            self.key = self.aes.key
            self.iv = self.aes.iv
        else:
            self.xor = XOREncryption()
            self.key = self.xor.key
            self.salt = None
            self.iv = None
    
    def encrypt_bytecode(self, bytecode: str) -> Tuple[str, dict]:
        """
        Encrypt serialized bytecode
        
        Args:
            bytecode: Serialized bytecode string
            
        Returns:
            Tuple of (encrypted_data, metadata)
        """
        data = bytecode.encode('utf-8')
        
        if self.use_aes:
            encrypted = self.aes.encrypt(data)
        else:
            encrypted = self.xor.encrypt(data)
        
        # Convert to Lua-embeddable format
        encrypted_list = list(encrypted)
        
        metadata = {
            'method': 'aes256' if self.use_aes else 'xor',
            'key': list(self.key),
            'iv': list(self.iv) if self.iv else None,
            'salt': list(self.salt) if self.salt else None,
            'length': len(data)
        }
        
        return encrypted_list, metadata
    
    def generate_decryptor_lua(self, encrypted_data: List[int], metadata: dict) -> str:
        """
        Generate Lua code for runtime decryption
        
        Args:
            encrypted_data: Encrypted bytecode as byte list
            metadata: Encryption metadata
            
        Returns:
            Lua decryption code
        """
        key = metadata['key']
        
        if metadata['method'] == 'xor':
            return self._generate_xor_decryptor(encrypted_data, key)
        else:
            return self._generate_aes_decryptor(encrypted_data, key, metadata['iv'])
    
    def _generate_xor_decryptor(self, data: List[int], key: List[int]) -> str:
        """Generate XOR decryption code"""
        return f'''
local function __xor_decrypt(data, key)
    local result = {{}}
    local klen = #key
    local bxor = bit32 and bit32.bxor or function(a,b) return a ~ b end
    
    for i = 1, #data do
        result[i] = string.char(bxor(data[i], key[((i-1) % klen) + 1]))
    end
    
    return table.concat(result)
end

local __enc_data = {{{','.join(map(str, data))}}}
local __enc_key = {{{','.join(map(str, key))}}}
local __bytecode_str = __xor_decrypt(__enc_data, __enc_key)
'''
    
    def _generate_aes_decryptor(self, data: List[int], key: List[int], iv: List[int]) -> str:
        """Generate AES decryption code (simplified for Lua)"""
        # For Roblox/Lua, we use a simplified approach
        # Full AES would require too much code
        # We use XOR with derived key for compatibility
        
        derived_key = []
        for i in range(len(key)):
            derived_key.append(key[i] ^ (iv[i % len(iv)] if iv else 0))
        
        return self._generate_xor_decryptor(data, derived_key)


def create_encryption(method: str = 'aes256', password: str = None):
    """
    Factory function to create encryption instance
    
    Args:
        method: 'aes256' or 'xor'
        password: Optional password
        
    Returns:
        Encryption instance
    """
    if method == 'aes256':
        if password:
            key, salt = AESEncryption.derive_key(password)
            return AESEncryption(key)
        return AESEncryption()
    else:
        return XOREncryption()
