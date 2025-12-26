import os
import io
import json
import time
import hashlib
import asyncio
import threading
import traceback
from functools import wraps
from datetime import datetime
from typing import Optional

# Flask
from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS

# Discord
import discord
from discord import app_commands
from discord.ext import commands

# Core modules
from core.lexer import LuaLexer
from core.parser import LuaParser
from core.compiler import BytecodeCompiler
from core.vm_generator import VMGenerator
from core.encryption import AESEncryption, XOREncryption, BytecodeEncryption
from core.obfuscator import LuaObfuscator, ObfuscationConfig, ObfuscationResult
from core.variable_renamer import VariableRenamer, RenamerConfig, NamingStyle
from core.junk_code import JunkCodeGenerator, JunkConfig, JunkType
from core.anti_tamper import AntiTamper, AntiTamperConfig

# ══════════════════════════════════════════════════════════════
#                     CONFIGURATION
# ══════════════════════════════════════════════════════════════

# Flask Config
app = Flask(__name__, static_folder='static', static_url_path='/static')
CORS(app)
app.config['MAX_CONTENT_LENGTH'] = 5 * 1024 * 1024  # 5MB
app.config['SECRET_KEY'] = os.environ.get('SECRET_KEY', 'luashield-secret-key')

# Discord Config
DISCORD_BOT_TOKEN = os.environ.get('DISCORD_BOT_TOKEN', '')
BOT_PREFIX = os.environ.get('BOT_PREFIX', '!')
EMBED_COLOR = 0x7289DA
MAX_FILE_SIZE = 1024 * 1024  # 1MB

# Rate limiting
rate_limit_cache = {}
RATE_LIMIT_REQUESTS = 30
RATE_LIMIT_WINDOW = 60

# Discord Bot Instance
intents = discord.Intents.default()
intents.message_content = True
bot = commands.Bot(command_prefix=BOT_PREFIX, intents=intents, help_command=None)

# Bot ready flag
bot_ready = False

# ══════════════════════════════════════════════════════════════
#                     PRESET CONFIGURATIONS
# ══════════════════════════════════════════════════════════════

PRESETS = {
    'light': {
        'name': '💡 Light',
        'description': 'Fast, minimal protection',
        'config': {
            'use_vm': True,
            'encrypt_strings': False,
            'encrypt_bytecode': False,
            'rename_variables': True,
            'add_junk_code': False,
            'junk_code_ratio': 0.1,
            'add_anti_tamper': False,
            'add_anti_dump': False,
            'mode': 'loadstring',
            'minify': True
        }
    },
    'medium': {
        'name': '⚡ Medium',
        'description': 'Balanced protection',
        'config': {
            'use_vm': True,
            'encrypt_strings': True,
            'encrypt_bytecode': True,
            'encryption_method': 'xor',
            'rename_variables': True,
            'add_junk_code': True,
            'junk_code_ratio': 0.25,
            'add_anti_tamper': True,
            'add_anti_dump': False,
            'mode': 'loadstring',
            'minify': True
        }
    },
    'heavy': {
        'name': '🔒 Heavy',
        'description': 'Strong protection',
        'config': {
            'use_vm': True,
            'encrypt_strings': True,
            'encrypt_bytecode': True,
            'encryption_method': 'xor',
            'rename_variables': True,
            'add_junk_code': True,
            'junk_code_ratio': 0.4,
            'add_anti_tamper': True,
            'add_anti_dump': True,
            'mode': 'loadstring',
            'minify': True
        }
    },
    'maximum': {
        'name': '🛡️ Maximum',
        'description': 'Maximum protection',
        'config': {
            'use_vm': True,
            'encrypt_strings': True,
            'encrypt_bytecode': True,
            'encryption_method': 'aes256',
            'rename_variables': True,
            'add_junk_code': True,
            'junk_code_ratio': 0.5,
            'add_anti_tamper': True,
            'add_anti_dump': True,
            'mode': 'loadstring',
            'minify': True
        }
    },
    'roblox': {
        'name': '🎮 Roblox',
        'description': 'Optimized for Roblox',
        'config': {
            'use_vm': True,
            'encrypt_strings': True,
            'encrypt_bytecode': True,
            'encryption_method': 'xor',
            'rename_variables': True,
            'add_junk_code': True,
            'junk_code_ratio': 0.3,
            'add_anti_tamper': True,
            'add_anti_dump': True,
            'mode': 'loadstring',
            'minify': True
        }
    }
}

# ══════════════════════════════════════════════════════════════
#                     SHARED HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════

def validate_lua_source(source: str) -> tuple:
    """Validate Lua source code"""
    if not source or not source.strip():
        return False, "Source code is empty"
    
    if len(source) > MAX_FILE_SIZE:
        return False, f"Source code exceeds maximum size (1MB)"
    
    if source.startswith('\x1bLua') or source.startswith('\x1b\x4c\x75\x61'):
        return False, "Luac bytecode is not supported. Please provide Lua source code."
    
    try:
        source.encode('utf-8')
    except UnicodeError:
        return False, "Source contains invalid characters"
    
    return True, None


def do_obfuscate(source: str, preset: str = 'medium') -> ObfuscationResult:
    """Perform obfuscation with given preset"""
    preset_config = PRESETS.get(preset, PRESETS['medium'])['config']
    config = ObfuscationConfig(**preset_config)
    obfuscator = LuaObfuscator(config)
    return obfuscator.obfuscate(source)


def format_bytes(size: int) -> str:
    """Format bytes to human readable"""
    for unit in ['B', 'KB', 'MB']:
        if size < 1024:
            return f"{size:.1f} {unit}"
        size /= 1024
    return f"{size:.1f} GB"


def rate_limit(f):
    """Rate limiting decorator for Flask"""
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
                    'error': 'Rate limit exceeded',
                    'retry_after': int(RATE_LIMIT_WINDOW - (current_time - window_start))
                }), 429
            else:
                rate_limit_cache[client_ip] = (requests + 1, window_start)
        else:
            rate_limit_cache[client_ip] = (1, current_time)
        
        return f(*args, **kwargs)
    return decorated_function


# ══════════════════════════════════════════════════════════════
#                     FLASK WEB ROUTES
# ══════════════════════════════════════════════════════════════

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
        'service': 'LuaShield VM Obfuscator',
        'bot_status': 'online' if bot_ready else 'starting',
        'bot_name': str(bot.user) if bot.user else 'Not connected'
    })


@app.route('/api/obfuscate', methods=['POST'])
@rate_limit
def api_obfuscate():
    """Main obfuscation endpoint"""
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({'success': False, 'error': 'No JSON data provided'}), 400
        
        source = data.get('source', '')
        options = data.get('options', {})
        
        # Validate
        is_valid, error = validate_lua_source(source)
        if not is_valid:
            return jsonify({'success': False, 'error': error}), 400
        
        # Build config
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
            add_watermark=True,
            watermark_text=options.get('watermark', 'Protected by LuaShield')
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
        return jsonify({'success': False, 'error': str(e)}), 500


@app.route('/api/validate', methods=['POST'])
@rate_limit
def api_validate():
    """Validate Lua syntax"""
    try:
        data = request.get_json()
        if not data:
            return jsonify({'valid': False, 'errors': ['No JSON data']}), 400
        
        source = data.get('source', '')
        is_valid, error = validate_lua_source(source)
        
        if not is_valid:
            return jsonify({'valid': False, 'errors': [error]}), 400
        
        # Parse
        lexer = LuaLexer(source)
        tokens = lexer.tokenize()
        parser = LuaParser(tokens)
        ast = parser.parse()
        
        all_errors = lexer.get_errors() + parser.get_errors()
        
        return jsonify({
            'valid': len(all_errors) == 0,
            'errors': all_errors,
            'stats': {
                'tokens': len(tokens),
                'lines': source.count('\n') + 1,
                'characters': len(source)
            }
        })
        
    except Exception as e:
        return jsonify({'valid': False, 'errors': [str(e)]}), 500


@app.route('/api/presets', methods=['GET'])
def api_presets():
    """Get available presets"""
    return jsonify({'presets': PRESETS})


@app.route('/api/bot-status', methods=['GET'])
def api_bot_status():
    """Get Discord bot status"""
    if bot.user:
        guilds = [{'id': str(g.id), 'name': g.name, 'members': g.member_count} 
                  for g in bot.guilds]
        return jsonify({
            'online': bot_ready,
            'username': str(bot.user),
            'user_id': str(bot.user.id),
            'guilds': len(guilds),
            'guild_list': guilds[:10]  # Top 10
        })
    return jsonify({'online': False, 'username': None})


@app.route('/api/stats', methods=['GET'])
def api_stats():
    """Get service statistics"""
    return jsonify({
        'service': 'LuaShield VM Obfuscator',
        'version': '2.1.0',
        'web': 'active',
        'bot': 'online' if bot_ready else 'offline',
        'bot_servers': len(bot.guilds) if bot.guilds else 0,
        'features': [
            'Real VM-based obfuscation',
            'AES-256 / XOR encryption',
            'Variable renaming',
            'Junk code injection',
            'Anti-tamper protection',
            'Anti-dump protection',
            'Discord bot integration',
            'Roblox Luau support'
        ]
    })


# Error handlers
@app.errorhandler(404)
def not_found(e):
    return jsonify({'error': 'Not found'}), 404

@app.errorhandler(500)
def internal_error(e):
    return jsonify({'error': 'Internal server error'}), 500

@app.errorhandler(413)
def too_large(e):
    return jsonify({'error': 'File too large (max 5MB)'}), 413


# ══════════════════════════════════════════════════════════════
#                     DISCORD BOT HELPERS
# ══════════════════════════════════════════════════════════════

def create_embed(title: str, description: str = None, color: int = EMBED_COLOR,
                 error: bool = False, success: bool = False) -> discord.Embed:
    """Create styled embed"""
    if error:
        color = 0xED4245
    elif success:
        color = 0x57F287
    
    embed = discord.Embed(title=title, description=description, color=color)
    embed.set_footer(text="LuaShield VM Obfuscator v2.1.0")
    embed.timestamp = datetime.utcnow()
    return embed


async def read_attachment(attachment: discord.Attachment) -> tuple:
    """Read Discord attachment"""
    if attachment.size > MAX_FILE_SIZE:
        return None, f"File too large (max {format_bytes(MAX_FILE_SIZE)})"
    
    filename = attachment.filename.lower()
    if not (filename.endswith('.lua') or filename.endswith('.txt')):
        return None, "Only .lua and .txt files supported"
    
    if filename.endswith('.luac'):
        return None, "Bytecode (.luac) not supported"
    
    try:
        content = await attachment.read()
        return content.decode('utf-8'), None
    except Exception as e:
        return None, f"Failed to read file: {e}"


# ══════════════════════════════════════════════════════════════
#                     DISCORD BOT UI COMPONENTS
# ══════════════════════════════════════════════════════════════

class QuickObfuscateView(discord.ui.View):
    """Quick preset buttons"""
    
    def __init__(self, source: str, author_id: int):
        super().__init__(timeout=180)
        self.source = source
        self.author_id = author_id
    
    async def interaction_check(self, interaction: discord.Interaction) -> bool:
        if interaction.user.id != self.author_id:
            await interaction.response.send_message("❌ Not your command", ephemeral=True)
            return False
        return True
    
    async def process(self, interaction: discord.Interaction, preset_key: str):
        await interaction.response.defer()
        preset = PRESETS[preset_key]
        
        embed = create_embed("⏳ Processing...", f"Using **{preset['name']}** preset...")
        await interaction.edit_original_response(embed=embed, view=None)
        
        try:
            result = do_obfuscate(self.source, preset_key)
            
            if result.success:
                stats = result.stats
                embed = create_embed(
                    "✅ Obfuscation Complete!",
                    f"**Preset:** {preset['name']}\n"
                    f"**Original:** {format_bytes(stats.get('original_size', 0))}\n"
                    f"**Output:** {format_bytes(stats.get('output_size', 0))}\n"
                    f"**Time:** {stats.get('time_ms', 0)}ms",
                    success=True
                )
                
                file = discord.File(
                    io.BytesIO(result.output.encode('utf-8')),
                    filename=f"obfuscated_{preset_key}_{interaction.id}.lua"
                )
                await interaction.edit_original_response(embed=embed, attachments=[file])
            else:
                embed = create_embed("❌ Failed", f"```{'; '.join(result.errors)}```", error=True)
                await interaction.edit_original_response(embed=embed)
                
        except Exception as e:
            embed = create_embed("❌ Error", f"```{e}```", error=True)
            await interaction.edit_original_response(embed=embed)
    
    @discord.ui.button(label="Light", emoji="💡", style=discord.ButtonStyle.secondary)
    async def light(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process(interaction, 'light')
    
    @discord.ui.button(label="Medium", emoji="⚡", style=discord.ButtonStyle.primary)
    async def medium(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process(interaction, 'medium')
    
    @discord.ui.button(label="Heavy", emoji="🔒", style=discord.ButtonStyle.primary)
    async def heavy(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process(interaction, 'heavy')
    
    @discord.ui.button(label="Maximum", emoji="🛡️", style=discord.ButtonStyle.danger)
    async def maximum(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process(interaction, 'maximum')
    
    @discord.ui.button(label="Roblox", emoji="🎮", style=discord.ButtonStyle.success)
    async def roblox(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process(interaction, 'roblox')


# ══════════════════════════════════════════════════════════════
#                     DISCORD BOT EVENTS
# ══════════════════════════════════════════════════════════════

@bot.event
async def on_ready():
    """Bot ready event"""
    global bot_ready
    bot_ready = True
    
    print(f"""
╔═══════════════════════════════════════════════════════════╗
║              🤖 Discord Bot Connected!                    ║
╠═══════════════════════════════════════════════════════════╣
║  Bot: {str(bot.user):<50} ║
║  ID: {str(bot.user.id):<51} ║
║  Servers: {len(bot.guilds):<46} ║
╚═══════════════════════════════════════════════════════════╝
    """)
    
    # Sync slash commands
    try:
        synced = await bot.tree.sync()
        print(f"✅ Synced {len(synced)} slash commands")
    except Exception as e:
        print(f"❌ Sync failed: {e}")
    
    # Set presence
    await bot.change_presence(
        activity=discord.Activity(
            type=discord.ActivityType.watching,
            name="/obfuscate | LuaShield"
        )
    )


# ══════════════════════════════════════════════════════════════
#                     DISCORD SLASH COMMANDS
# ══════════════════════════════════════════════════════════════

@bot.tree.command(name="obfuscate", description="Obfuscate Lua code")
@app_commands.describe(
    preset="Obfuscation preset",
    file="Lua file (.lua or .txt)",
    code="Lua code (if no file)"
)
@app_commands.choices(preset=[
    app_commands.Choice(name="💡 Light", value="light"),
    app_commands.Choice(name="⚡ Medium", value="medium"),
    app_commands.Choice(name="🔒 Heavy", value="heavy"),
    app_commands.Choice(name="🛡️ Maximum", value="maximum"),
    app_commands.Choice(name="🎮 Roblox", value="roblox"),
])
async def slash_obfuscate(
    interaction: discord.Interaction,
    preset: str = "medium",
    file: discord.Attachment = None,
    code: str = None
):
    await interaction.response.defer()
    
    source = None
    
    if file:
        content, error = await read_attachment(file)
        if error:
            embed = create_embed("❌ Error", error, error=True)
            await interaction.followup.send(embed=embed)
            return
        source = content
    elif code:
        source = code
    else:
        embed = create_embed(
            "❌ No Input",
            "Provide a **file** or **code**\n\n"
            "`/obfuscate file:script.lua`\n"
            "`/obfuscate code:print('Hello')`",
            error=True
        )
        await interaction.followup.send(embed=embed)
        return
    
    is_valid, error = validate_lua_source(source)
    if not is_valid:
        embed = create_embed("❌ Invalid", error, error=True)
        await interaction.followup.send(embed=embed)
        return
    
    try:
        result = do_obfuscate(source, preset)
        preset_info = PRESETS[preset]
        
        if result.success:
            stats = result.stats
            embed = create_embed(
                "✅ Obfuscation Complete!",
                f"**Preset:** {preset_info['name']}\n"
                f"**Original:** {format_bytes(stats.get('original_size', 0))}\n"
                f"**Output:** {format_bytes(stats.get('output_size', 0))}\n"
                f"**Ratio:** {stats.get('compression_ratio', 0)}%\n"
                f"**Time:** {stats.get('time_ms', 0)}ms",
                success=True
            )
            
            file_obj = discord.File(
                io.BytesIO(result.output.encode('utf-8')),
                filename=f"obfuscated_{preset}_{interaction.id}.lua"
            )
            await interaction.followup.send(embed=embed, file=file_obj)
        else:
            embed = create_embed("❌ Failed", f"```{'; '.join(result.errors)}```", error=True)
            await interaction.followup.send(embed=embed)
            
    except Exception as e:
        embed = create_embed("❌ Error", f"```{e}```", error=True)
        await interaction.followup.send(embed=embed)


@bot.tree.command(name="obf", description="Quick obfuscate with button selection")
@app_commands.describe(file="Lua file to obfuscate")
async def slash_obf(interaction: discord.Interaction, file: discord.Attachment):
    await interaction.response.defer()
    
    content, error = await read_attachment(file)
    if error:
        embed = create_embed("❌ Error", error, error=True)
        await interaction.followup.send(embed=embed)
        return
    
    is_valid, error = validate_lua_source(content)
    if not is_valid:
        embed = create_embed("❌ Invalid", error, error=True)
        await interaction.followup.send(embed=embed)
        return
    
    embed = create_embed(
        "🔧 Select Preset",
        f"**File:** `{file.filename}`\n"
        f"**Size:** {format_bytes(len(content))}\n\n"
        "Choose obfuscation level:"
    )
    
    view = QuickObfuscateView(content, interaction.user.id)
    await interaction.followup.send(embed=embed, view=view)


@bot.tree.command(name="presets", description="View obfuscation presets")
async def slash_presets(interaction: discord.Interaction):
    embed = create_embed("🔧 Obfuscation Presets")
    
    for key, preset in PRESETS.items():
        config = preset['config']
        features = []
        if config.get('encrypt_strings'): features.append("🔐")
        if config.get('rename_variables'): features.append("🔤")
        if config.get('add_junk_code'): features.append("🧬")
        if config.get('add_anti_tamper'): features.append("🛡️")
        
        embed.add_field(
            name=f"{preset['name']}",
            value=f"{preset['description']}\n{' '.join(features)}",
            inline=True
        )
    
    await interaction.response.send_message(embed=embed)


@bot.tree.command(name="help", description="Show help")
async def slash_help(interaction: discord.Interaction):
    embed = create_embed(
        "🛡️ LuaShield Help",
        "Lua VM Obfuscator with Discord integration"
    )
    
    embed.add_field(
        name="📌 Commands",
        value=(
            "`/obfuscate` - Obfuscate with preset\n"
            "`/obf` - Quick obfuscate\n"
            "`/presets` - View presets\n"
            "`/help` - This help"
        ),
        inline=False
    )
    
    embed.add_field(
        name="📁 Supported",
        value="✅ `.lua` `.txt`\n❌ `.luac`",
        inline=True
    )
    
    embed.add_field(
        name="✨ Features",
        value="VM, AES-256, Anti-tamper",
        inline=True
    )
    
    await interaction.response.send_message(embed=embed)


# ══════════════════════════════════════════════════════════════
#                     DISCORD PREFIX COMMANDS
# ══════════════════════════════════════════════════════════════

@bot.command(name='obf', aliases=['obfuscate', 'protect'])
async def prefix_obf(ctx: commands.Context, preset: str = 'medium', *, code: str = None):
    """Prefix command: !obf <preset> <code>"""
    source = None
    
    if ctx.message.attachments:
        content, error = await read_attachment(ctx.message.attachments[0])
        if error:
            await ctx.reply(embed=create_embed("❌ Error", error, error=True))
            return
        source = content
    elif code:
        if preset not in PRESETS:
            code = f"{preset} {code}" if code else preset
            preset = 'medium'
        source = code
    else:
        embed = create_embed(
            "📋 Usage",
            f"`{BOT_PREFIX}obf <preset> <code>`\n"
            f"`{BOT_PREFIX}obf medium print('test')`\n\n"
            f"Or attach a `.lua` file",
        )
        await ctx.reply(embed=embed)
        return
    
    if preset not in PRESETS:
        preset = 'medium'
    
    is_valid, error = validate_lua_source(source)
    if not is_valid:
        await ctx.reply(embed=create_embed("❌ Invalid", error, error=True))
        return
    
    msg = await ctx.reply(embed=create_embed("⏳ Processing...", f"Using **{PRESETS[preset]['name']}**"))
    
    try:
        result = do_obfuscate(source, preset)
        
        if result.success:
            stats = result.stats
            embed = create_embed(
                "✅ Complete!",
                f"**Preset:** {PRESETS[preset]['name']}\n"
                f"**Original:** {format_bytes(stats.get('original_size', 0))}\n"
                f"**Output:** {format_bytes(stats.get('output_size', 0))}",
                success=True
            )
            
            file = discord.File(
                io.BytesIO(result.output.encode('utf-8')),
                filename=f"obfuscated_{preset}_{ctx.message.id}.lua"
            )
            await msg.edit(embed=embed, attachments=[file])
        else:
            await msg.edit(embed=create_embed("❌ Failed", f"```{result.errors}```", error=True))
            
    except Exception as e:
        await msg.edit(embed=create_embed("❌ Error", f"```{e}```", error=True))


@bot.command(name='help', aliases=['h'])
async def prefix_help(ctx: commands.Context):
    embed = create_embed(
        "🛡️ LuaShield Help",
        f"Prefix: `{BOT_PREFIX}`"
    )
    embed.add_field(
        name="Commands",
        value=f"`{BOT_PREFIX}obf <preset> <code>`\n`/obfuscate`\n`/obf`",
        inline=False
    )
    await ctx.reply(embed=embed)


# ══════════════════════════════════════════════════════════════
#                     BOT ERROR HANDLERS
# ══════════════════════════════════════════════════════════════

@bot.event
async def on_command_error(ctx, error):
    if isinstance(error, commands.CommandNotFound):
        return
    print(f"Command error: {error}")


@bot.tree.error
async def on_app_error(interaction, error):
    print(f"Slash error: {error}")
    embed = create_embed("❌ Error", str(error), error=True)
    if interaction.response.is_done():
        await interaction.followup.send(embed=embed, ephemeral=True)
    else:
        await interaction.response.send_message(embed=embed, ephemeral=True)


# ══════════════════════════════════════════════════════════════
#                     RUN BOT IN BACKGROUND
# ══════════════════════════════════════════════════════════════

def run_discord_bot():
    """Run Discord bot in background thread"""
    if not DISCORD_BOT_TOKEN:
        print("⚠️ DISCORD_BOT_TOKEN not set - Bot disabled")
        return
    
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    
    try:
        loop.run_until_complete(bot.start(DISCORD_BOT_TOKEN))
    except Exception as e:
        print(f"❌ Bot error: {e}")


# ══════════════════════════════════════════════════════════════
#                     MAIN ENTRY POINT
# ══════════════════════════════════════════════════════════════

def main():
    port = int(os.environ.get('PORT', 5000))
    
    print(f"""
╔═══════════════════════════════════════════════════════════╗
║         🛡️ LuaShield VM Obfuscator v2.1.0                 ║
║            Web + Discord Bot Integrated                   ║
╠═══════════════════════════════════════════════════════════╣
║  Web Server: http://0.0.0.0:{port:<28} ║
║  Bot Token: {'✅ Set' if DISCORD_BOT_TOKEN else '❌ Not set':<40} ║
╠═══════════════════════════════════════════════════════════╣
║  Web Endpoints:                                           ║
║    GET  /              - Web UI                           ║
║    POST /api/obfuscate - Obfuscate API                    ║
║    GET  /api/health    - Health check                     ║
║    GET  /api/bot-status - Bot status                      ║
╠═══════════════════════════════════════════════════════════╣
║  Bot Commands:                                            ║
║    /obfuscate - Obfuscate with preset                     ║
║    /obf       - Quick obfuscate                           ║
║    !obf       - Prefix command                            ║
╚═══════════════════════════════════════════════════════════╝
    """)
    
    # Start Discord bot in background thread
    if DISCORD_BOT_TOKEN:
        bot_thread = threading.Thread(target=run_discord_bot, daemon=True)
        bot_thread.start()
        print("🤖 Starting Discord bot in background...")
    
    # Run Flask (main thread)
    app.run(host='0.0.0.0', port=port, threaded=True)


if __name__ == '__main__':
    main()
