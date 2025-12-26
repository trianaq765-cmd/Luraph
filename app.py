import os
import json
import time
import hashlib
import traceback
from functools import wraps
from flask import Flask, request, jsonify, send_from_directory, render_template_string
from flask_cors import CORS

# Import core modules
from core.lexer import LuaLexer
from core.parser import LuaParser
from core.compiler import BytecodeCompiler
from core.vm_generator import VMGenerator
from core.encryption import AESEncryption, XOREncryption, BytecodeEncryption
from core.obfuscator import LuaObfuscator, ObfuscationConfig, ObfuscationResult
from core.variable_renamer import VariableRenamer, RenamerConfig, NamingStyle
from core.junk_code import JunkCodeGenerator, JunkConfig, JunkType
from core.anti_tamper import AntiTamper, AntiTamperConfig

# Initialize Flask app
app = Flask(__name__, static_folder='static', static_url_path='/static')
CORS(app)

# Configuration
app.config['MAX_CONTENT_LENGTH'] = 5 * 1024 * 1024  # 5MB max
app.config['SECRET_KEY'] = os.environ.get('SECRET_KEY', 'luashield-dev-key-change-in-production')

# Rate limiting (simple in-memory)
rate_limit_cache = {}
RATE_LIMIT_REQUESTS = 30
RATE_LIMIT_WINDOW = 60  # seconds


def rate_limit(f):
    """Simple rate limiting decorator"""
    @wraps(f)
    def decorated_function(*args, **kwargs):
        client_ip = request.remote_addr
        current_time = time.time()
        
        if client_ip in rate_limit_cache:
            requests, window_start = rate_limit_cache[client_ip]
            
            if current_time - window_start > RATE_LIMIT_WINDOW:
                rate_limit_cache[client_ip] = (1, current_time)
            elif requests >= RATE_LIMIT_REQUESTS:
                return jsonify({
                    'success': False,
                    'error': 'Rate limit exceeded. Please wait before making more requests.',
                    'retry_after': int(RATE_LIMIT_WINDOW - (current_time - window_start))
                }), 429
            else:
                rate_limit_cache[client_ip] = (requests + 1, window_start)
        else:
            rate_limit_cache[client_ip] = (1, current_time)
        
        return f(*args, **kwargs)
    return decorated_function


def validate_lua_source(source: str) -> tuple:
    """
    Validate Lua source code
    
    Returns:
        Tuple of (is_valid, error_message)
    """
    if not source or not source.strip():
        return False, "Source code is empty"
    
    if len(source) > 1024 * 1024:  # 1MB limit
        return False, "Source code exceeds maximum size (1MB)"
    
    # Check for bytecode
    if source.startswith('\x1bLua') or source.startswith('\x1b\x4c\x75\x61'):
        return False, "Luac bytecode is not supported. Please provide Lua source code (.lua or .txt)"
    
    # Basic syntax check
    try:
        source.encode('utf-8')
    except UnicodeError:
        return False, "Source contains invalid characters"
    
    return True, None


@app.route('/')
def index():
    """Serve main page"""
    return send_from_directory('static', 'index.html')


@app.route('/api/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    return jsonify({
        'status': 'healthy',
        'version': '2.1.0',
        'service': 'LuaShield VM Obfuscator'
    })


@app.route('/api/obfuscate', methods=['POST'])
@rate_limit
def obfuscate():
    """
    Main obfuscation endpoint
    
    Request JSON:
    {
        "source": "lua source code",
        "options": {
            "use_vm": true,
            "encrypt_strings": true,
            "encrypt_bytecode": true,
            "encryption_method": "xor",
            "rename_variables": true,
            "variable_style": "random",
            "add_junk_code": true,
            "junk_intensity": 0.3,
            "add_anti_tamper": true,
            "add_anti_dump": true,
            "mode": "loadstring",
            "minify": true,
            "watermark": "Protected by LuaShield"
        }
    }
    
    Response JSON:
    {
        "success": true,
        "output": "obfuscated code",
        "stats": {...},
        "warnings": [...]
    }
    """
    try:
        # Parse request
        data = request.get_json()
        
        if not data:
            return jsonify({
                'success': False,
                'error': 'No JSON data provided'
            }), 400
        
        source = data.get('source', '')
        options = data.get('options', {})
        
        # Validate source
        is_valid, error = validate_lua_source(source)
        if not is_valid:
            return jsonify({
                'success': False,
                'error': error
            }), 400
        
        # Build configuration
        config = ObfuscationConfig(
            use_vm=options.get('use_vm', True),
            encrypt_strings=options.get('encrypt_strings', True),
            encrypt_bytecode=options.get('encrypt_bytecode', True),
            encryption_method=options.get('encryption_method', 'xor'),
            rename_variables=options.get('rename_variables', True),
            add_junk_code=options.get('add_junk_code', True),
            junk_code_ratio=options.get('junk_intensity', 0.3),
            add_anti_tamper=options.get('add_anti_tamper', True),
            add_anti_dump=options.get('add_anti_dump', True),
            mode=options.get('mode', 'loadstring'),
            minify=options.get('minify', True),
            add_watermark=options.get('watermark', True) if isinstance(options.get('watermark'), bool) else True,
            watermark_text=options.get('watermark', 'Protected by LuaShield') if isinstance(options.get('watermark'), str) else 'Protected by LuaShield'
        )
        
        # Obfuscate
        obfuscator = LuaObfuscator(config)
        result = obfuscator.obfuscate(source)
        
        if result.success:
            return jsonify({
                'success': True,
                'output': result.output,
                'stats': result.stats,
                'warnings': result.warnings
            })
        else:
            return jsonify({
                'success': False,
                'error': '; '.join(result.errors),
                'warnings': result.warnings
            }), 400
            
    except Exception as e:
        app.logger.error(f"Obfuscation error: {traceback.format_exc()}")
        return jsonify({
            'success': False,
            'error': f'Internal error: {str(e)}'
        }), 500


@app.route('/api/validate', methods=['POST'])
@rate_limit
def validate():
    """
    Validate Lua source code syntax
    
    Request JSON:
    {
        "source": "lua source code"
    }
    
    Response JSON:
    {
        "valid": true,
        "errors": [],
        "warnings": [],
        "stats": {...}
    }
    """
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({
                'valid': False,
                'errors': ['No JSON data provided']
            }), 400
        
        source = data.get('source', '')
        
        # Basic validation
        is_valid, error = validate_lua_source(source)
        if not is_valid:
            return jsonify({
                'valid': False,
                'errors': [error]
            }), 400
        
        # Lexical analysis
        lexer = LuaLexer(source)
        tokens = lexer.tokenize()
        lexer_errors = lexer.get_errors()
        
        # Parsing
        parser = LuaParser(tokens)
        ast = parser.parse()
        parser_errors = parser.get_errors()
        
        all_errors = lexer_errors + parser_errors
        
        # Stats
        stats = {
            'tokens': len(tokens),
            'lines': source.count('\n') + 1,
            'characters': len(source)
        }
        
        return jsonify({
            'valid': len(all_errors) == 0,
            'errors': all_errors,
            'warnings': [],
            'stats': stats
        })
        
    except Exception as e:
        return jsonify({
            'valid': False,
            'errors': [str(e)]
        }), 500


@app.route('/api/analyze', methods=['POST'])
@rate_limit
def analyze():
    """
    Analyze Lua source code structure
    
    Request JSON:
    {
        "source": "lua source code"
    }
    
    Response JSON:
    {
        "success": true,
        "analysis": {
            "functions": [...],
            "variables": [...],
            "complexity": {...}
        }
    }
    """
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({
                'success': False,
                'error': 'No JSON data provided'
            }), 400
        
        source = data.get('source', '')
        
        # Validate
        is_valid, error = validate_lua_source(source)
        if not is_valid:
            return jsonify({
                'success': False,
                'error': error
            }), 400
        
        # Analyze
        lexer = LuaLexer(source)
        tokens = lexer.tokenize()
        
        parser = LuaParser(tokens)
        ast = parser.parse()
        
        # Extract information
        analysis = {
            'tokens': len(tokens),
            'lines': source.count('\n') + 1,
            'characters': len(source),
            'functions': [],
            'variables': [],
            'strings': 0,
            'numbers': 0,
            'comments_estimated': source.count('--')
        }
        
        # Count token types
        from core.lexer import TokenType
        for token in tokens:
            if token.type == TokenType.STRING:
                analysis['strings'] += 1
            elif token.type == TokenType.NUMBER:
                analysis['numbers'] += 1
            elif token.type == TokenType.FUNCTION:
                analysis['functions'].append({
                    'line': token.line,
                    'column': token.column
                })
            elif token.type == TokenType.IDENTIFIER:
                if token.value not in analysis['variables']:
                    analysis['variables'].append(token.value)
        
        # Limit variable list
        analysis['variables'] = analysis['variables'][:50]
        analysis['variable_count'] = len(analysis['variables'])
        
        return jsonify({
            'success': True,
            'analysis': analysis
        })
        
    except Exception as e:
        return jsonify({
            'success': False,
            'error': str(e)
        }), 500


@app.route('/api/presets', methods=['GET'])
def get_presets():
    """
    Get obfuscation presets
    
    Response JSON:
    {
        "presets": {
            "light": {...},
            "medium": {...},
            "heavy": {...},
            "maximum": {...}
        }
    }
    """
    presets = {
        'light': {
            'name': 'Light',
            'description': 'Fast obfuscation with minimal protection',
            'options': {
                'use_vm': True,
                'encrypt_strings': False,
                'encrypt_bytecode': False,
                'rename_variables': True,
                'variable_style': 'random',
                'add_junk_code': False,
                'junk_intensity': 0.1,
                'add_anti_tamper': False,
                'add_anti_dump': False,
                'mode': 'loadstring',
                'minify': True
            }
        },
        'medium': {
            'name': 'Medium',
            'description': 'Balanced protection and performance',
            'options': {
                'use_vm': True,
                'encrypt_strings': True,
                'encrypt_bytecode': True,
                'encryption_method': 'xor',
                'rename_variables': True,
                'variable_style': 'random',
                'add_junk_code': True,
                'junk_intensity': 0.2,
                'add_anti_tamper': True,
                'add_anti_dump': False,
                'mode': 'loadstring',
                'minify': True
            }
        },
        'heavy': {
            'name': 'Heavy',
            'description': 'Strong protection for sensitive code',
            'options': {
                'use_vm': True,
                'encrypt_strings': True,
                'encrypt_bytecode': True,
                'encryption_method': 'xor',
                'rename_variables': True,
                'variable_style': 'similar',
                'add_junk_code': True,
                'junk_intensity': 0.4,
                'add_anti_tamper': True,
                'add_anti_dump': True,
                'mode': 'loadstring',
                'minify': True
            }
        },
        'maximum': {
            'name': 'Maximum',
            'description': 'Maximum protection (larger output size)',
            'options': {
                'use_vm': True,
                'encrypt_strings': True,
                'encrypt_bytecode': True,
                'encryption_method': 'aes256',
                'rename_variables': True,
                'variable_style': 'similar',
                'add_junk_code': True,
                'junk_intensity': 0.5,
                'add_anti_tamper': True,
                'add_anti_dump': True,
                'mode': 'loadstring',
                'minify': True
            }
        },
        'roblox': {
            'name': 'Roblox Optimized',
            'description': 'Optimized for Roblox executors',
            'options': {
                'use_vm': True,
                'encrypt_strings': True,
                'encrypt_bytecode': True,
                'encryption_method': 'xor',
                'rename_variables': True,
                'variable_style': 'random',
                'add_junk_code': True,
                'junk_intensity': 0.25,
                'add_anti_tamper': True,
                'add_anti_dump': True,
                'mode': 'loadstring',
                'minify': True
            }
        }
    }
    
    return jsonify({'presets': presets})


@app.route('/api/options', methods=['GET'])
def get_options():
    """
    Get available obfuscation options
    
    Response JSON:
    {
        "options": {...}
    }
    """
    options = {
        'encryption_methods': [
            {'value': 'xor', 'label': 'XOR (Fast)', 'description': 'Fast encryption, good for most cases'},
            {'value': 'aes256', 'label': 'AES-256 (Strong)', 'description': 'Strong encryption, slightly larger output'}
        ],
        'variable_styles': [
            {'value': 'random', 'label': 'Random', 'description': 'Random alphanumeric names'},
            {'value': 'underscore', 'label': 'Underscore', 'description': 'Underscore-heavy names'},
            {'value': 'similar', 'label': 'Similar Chars', 'description': 'Confusing similar characters (l, I, 1, O, 0)'},
            {'value': 'hex', 'label': 'Hex', 'description': 'Hexadecimal-style names'}
        ],
        'modes': [
            {'value': 'loadstring', 'label': 'Loadstring', 'description': 'Returns function, use with loadstring'},
            {'value': 'raw', 'label': 'Raw', 'description': 'Direct execution, no return'}
        ],
        'junk_intensity': {
            'min': 0,
            'max': 1,
            'step': 0.1,
            'default': 0.3
        }
    }
    
    return jsonify({'options': options})


@app.route('/api/stats', methods=['GET'])
def get_stats():
    """Get service statistics (public)"""
    return jsonify({
        'service': 'LuaShield VM Obfuscator',
        'version': '2.1.0',
        'features': [
            'Real VM-based obfuscation',
            'AES-256 / XOR encryption',
            'Variable renaming',
            'Junk code injection',
            'Anti-tamper protection',
            'Anti-dump protection',
            'Roblox Luau support',
            'Loadstring compatible'
        ],
        'limits': {
            'max_file_size': '1MB',
            'rate_limit': f'{RATE_LIMIT_REQUESTS} requests per {RATE_LIMIT_WINDOW}s'
        },
        'supported_input': ['.lua', '.txt', 'raw text'],
        'not_supported': ['.luac (bytecode)']
    })


# Error handlers
@app.errorhandler(404)
def not_found(e):
    return jsonify({'error': 'Endpoint not found'}), 404


@app.errorhandler(500)
def internal_error(e):
    return jsonify({'error': 'Internal server error'}), 500


@app.errorhandler(413)
def too_large(e):
    return jsonify({'error': 'File too large. Maximum size is 5MB'}), 413


# CLI support
def obfuscate_file(input_path: str, output_path: str = None, **options):
    """
    Obfuscate a Lua file (for CLI usage)
    
    Args:
        input_path: Path to input file
        output_path: Path to output file
        **options: Obfuscation options
    """
    # Check file extension
    if input_path.endswith('.luac'):
        raise ValueError("Luac bytecode files are not supported. Please use .lua or .txt files.")
    
    # Read input
    with open(input_path, 'r', encoding='utf-8') as f:
        source = f.read()
    
    # Configure
    config = ObfuscationConfig(**options)
    
    # Obfuscate
    obfuscator = LuaObfuscator(config)
    result = obfuscator.obfuscate(source)
    
    if not result.success:
        raise ValueError(f"Obfuscation failed: {'; '.join(result.errors)}")
    
    # Write output
    if output_path:
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(result.output)
        print(f"Output written to: {output_path}")
    else:
        print(result.output)
    
    # Print stats
    print(f"\nStats: {result.stats}")
    
    if result.warnings:
        print(f"Warnings: {result.warnings}")
    
    return result


if __name__ == '__main__':
    import sys
    
    if len(sys.argv) > 1:
        # CLI mode
        if sys.argv[1] == '--help':
            print("""
LuaShield VM Obfuscator

Usage:
    python app.py                    # Start web server
    python app.py input.lua          # Obfuscate file (output to stdout)
    python app.py input.lua out.lua  # Obfuscate file (output to file)

Supported input: .lua, .txt files
NOT supported: .luac bytecode files
            """)
        else:
            input_file = sys.argv[1]
            output_file = sys.argv[2] if len(sys.argv) > 2 else None
            
            try:
                obfuscate_file(input_file, output_file)
            except Exception as e:
                print(f"Error: {e}")
                sys.exit(1)
    else:
        # Web server mode
        port = int(os.environ.get('PORT', 5000))
        debug = os.environ.get('FLASK_DEBUG', 'false').lower() == 'true'
        
        print(f"""
╔═══════════════════════════════════════════════════════════╗
║           LuaShield VM Obfuscator v2.1.0                  ║
╠═══════════════════════════════════════════════════════════╣
║  Server starting on http://0.0.0.0:{port:<5}                  ║
║  Debug mode: {str(debug):<5}                                    ║
║                                                           ║
║  Endpoints:                                               ║
║    GET  /              - Web interface                    ║
║    POST /api/obfuscate - Obfuscate code                   ║
║    POST /api/validate  - Validate syntax                  ║
║    POST /api/analyze   - Analyze code                     ║
║    GET  /api/presets   - Get presets                      ║
║    GET  /api/options   - Get options                      ║
║    GET  /api/health    - Health check                     ║
╚═══════════════════════════════════════════════════════════╝
        """)
        
        app.run(host='0.0.0.0', port=port, debug=debug)
