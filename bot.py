import os
import io
import asyncio
import aiohttp
from datetime import datetime
from typing import Optional
import discord
from discord import app_commands
from discord.ext import commands

# Import core modules
from core.obfuscator import LuaObfuscator, ObfuscationConfig, ObfuscationResult

# ==================== CONFIGURATION ====================
BOT_TOKEN = os.environ.get('DISCORD_BOT_TOKEN', '')
BOT_PREFIX = os.environ.get('BOT_PREFIX', '!')
EMBED_COLOR = 0x7289DA
MAX_FILE_SIZE = 1024 * 1024  # 1MB
MAX_MESSAGE_LENGTH = 1900

# ==================== BOT SETUP ====================
intents = discord.Intents.default()
intents.message_content = True

bot = commands.Bot(command_prefix=BOT_PREFIX, intents=intents, help_command=None)

# ==================== PRESET CONFIGURATIONS ====================
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

# ==================== HELPER FUNCTIONS ====================
def create_embed(title: str, description: str = None, color: int = EMBED_COLOR, 
                 error: bool = False, success: bool = False) -> discord.Embed:
    """Create a styled embed"""
    if error:
        color = 0xED4245
    elif success:
        color = 0x57F287
    
    embed = discord.Embed(title=title, description=description, color=color)
    embed.set_footer(text="LuaShield VM Obfuscator v2.1.0", 
                     icon_url="https://i.imgur.com/AfFp7pu.png")
    embed.timestamp = datetime.utcnow()
    return embed


def format_bytes(size: int) -> str:
    """Format bytes to human readable"""
    for unit in ['B', 'KB', 'MB']:
        if size < 1024:
            return f"{size:.1f} {unit}"
        size /= 1024
    return f"{size:.1f} GB"


def validate_lua_source(source: str) -> tuple:
    """Validate Lua source code"""
    if not source or not source.strip():
        return False, "Source code is empty"
    
    if len(source) > MAX_FILE_SIZE:
        return False, f"Source code exceeds maximum size ({format_bytes(MAX_FILE_SIZE)})"
    
    if source.startswith('\x1bLua') or source.startswith('\x1b\x4c\x75\x61'):
        return False, "Luac bytecode is not supported. Please provide Lua source code."
    
    return True, None


async def obfuscate_code(source: str, preset: str = 'medium') -> ObfuscationResult:
    """Obfuscate Lua code with given preset"""
    preset_config = PRESETS.get(preset, PRESETS['medium'])['config']
    config = ObfuscationConfig(**preset_config)
    obfuscator = LuaObfuscator(config)
    return obfuscator.obfuscate(source)


async def read_attachment(attachment: discord.Attachment) -> tuple:
    """Read content from Discord attachment"""
    if attachment.size > MAX_FILE_SIZE:
        return None, f"File too large. Maximum size is {format_bytes(MAX_FILE_SIZE)}"
    
    filename = attachment.filename.lower()
    if not (filename.endswith('.lua') or filename.endswith('.txt')):
        return None, "Only `.lua` and `.txt` files are supported"
    
    if filename.endswith('.luac'):
        return None, "Luac bytecode files are not supported"
    
    try:
        content = await attachment.read()
        return content.decode('utf-8'), None
    except Exception as e:
        return None, f"Failed to read file: {str(e)}"


# ==================== VIEWS (UI COMPONENTS) ====================
class PresetSelectView(discord.ui.View):
    """Dropdown view for selecting preset"""
    
    def __init__(self, source: str, author_id: int):
        super().__init__(timeout=120)
        self.source = source
        self.author_id = author_id
        self.add_item(PresetSelect(source, author_id))
    
    async def interaction_check(self, interaction: discord.Interaction) -> bool:
        if interaction.user.id != self.author_id:
            await interaction.response.send_message(
                "❌ Only the command author can use this menu.", 
                ephemeral=True
            )
            return False
        return True


class PresetSelect(discord.ui.Select):
    """Dropdown for preset selection"""
    
    def __init__(self, source: str, author_id: int):
        self.source = source
        self.author_id = author_id
        
        options = [
            discord.SelectOption(
                label=preset['name'].replace('💡 ', '').replace('⚡ ', '').replace('🔒 ', '').replace('🛡️ ', '').replace('🎮 ', ''),
                description=preset['description'],
                value=key,
                emoji=preset['name'].split()[0]
            )
            for key, preset in PRESETS.items()
        ]
        
        super().__init__(
            placeholder="🔧 Select obfuscation preset...",
            options=options,
            min_values=1,
            max_values=1
        )
    
    async def callback(self, interaction: discord.Interaction):
        await interaction.response.defer()
        
        preset_key = self.values[0]
        preset = PRESETS[preset_key]
        
        # Processing message
        embed = create_embed(
            "⏳ Processing...",
            f"Obfuscating with **{preset['name']}** preset..."
        )
        await interaction.edit_original_response(embed=embed, view=None)
        
        # Obfuscate
        try:
            result = await obfuscate_code(self.source, preset_key)
            
            if result.success:
                # Create stats embed
                stats = result.stats
                embed = create_embed(
                    "✅ Obfuscation Complete!",
                    f"**Preset:** {preset['name']}\n"
                    f"**Original:** {format_bytes(stats.get('original_size', 0))}\n"
                    f"**Output:** {format_bytes(stats.get('output_size', 0))}\n"
                    f"**Ratio:** {stats.get('compression_ratio', 0)}%\n"
                    f"**Time:** {stats.get('time_ms', 0)}ms",
                    success=True
                )
                
                # Add warnings if any
                if result.warnings:
                    embed.add_field(
                        name="⚠️ Warnings",
                        value="\n".join(result.warnings[:3]),
                        inline=False
                    )
                
                # Create file
                output_bytes = result.output.encode('utf-8')
                file = discord.File(
                    io.BytesIO(output_bytes),
                    filename=f"obfuscated_{preset_key}_{interaction.id}.lua"
                )
                
                await interaction.edit_original_response(embed=embed, attachments=[file])
            else:
                embed = create_embed(
                    "❌ Obfuscation Failed",
                    f"```\n{'; '.join(result.errors)}\n```",
                    error=True
                )
                await interaction.edit_original_response(embed=embed)
                
        except Exception as e:
            embed = create_embed(
                "❌ Error",
                f"```\n{str(e)}\n```",
                error=True
            )
            await interaction.edit_original_response(embed=embed)


class QuickObfuscateView(discord.ui.View):
    """Quick buttons for common presets"""
    
    def __init__(self, source: str, author_id: int):
        super().__init__(timeout=120)
        self.source = source
        self.author_id = author_id
    
    async def interaction_check(self, interaction: discord.Interaction) -> bool:
        if interaction.user.id != self.author_id:
            await interaction.response.send_message(
                "❌ Only the command author can use this.", 
                ephemeral=True
            )
            return False
        return True
    
    async def process_obfuscation(self, interaction: discord.Interaction, preset_key: str):
        await interaction.response.defer()
        
        preset = PRESETS[preset_key]
        
        embed = create_embed(
            "⏳ Processing...",
            f"Obfuscating with **{preset['name']}** preset..."
        )
        await interaction.edit_original_response(embed=embed, view=None)
        
        try:
            result = await obfuscate_code(self.source, preset_key)
            
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
                
                output_bytes = result.output.encode('utf-8')
                file = discord.File(
                    io.BytesIO(output_bytes),
                    filename=f"obfuscated_{preset_key}_{interaction.id}.lua"
                )
                
                await interaction.edit_original_response(embed=embed, attachments=[file])
            else:
                embed = create_embed(
                    "❌ Obfuscation Failed",
                    f"```\n{'; '.join(result.errors)}\n```",
                    error=True
                )
                await interaction.edit_original_response(embed=embed)
                
        except Exception as e:
            embed = create_embed(
                "❌ Error",
                f"```\n{str(e)}\n```",
                error=True
            )
            await interaction.edit_original_response(embed=embed)
    
    @discord.ui.button(label="Light", emoji="💡", style=discord.ButtonStyle.secondary)
    async def light_button(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process_obfuscation(interaction, 'light')
    
    @discord.ui.button(label="Medium", emoji="⚡", style=discord.ButtonStyle.primary)
    async def medium_button(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process_obfuscation(interaction, 'medium')
    
    @discord.ui.button(label="Heavy", emoji="🔒", style=discord.ButtonStyle.primary)
    async def heavy_button(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process_obfuscation(interaction, 'heavy')
    
    @discord.ui.button(label="Maximum", emoji="🛡️", style=discord.ButtonStyle.danger)
    async def maximum_button(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process_obfuscation(interaction, 'maximum')
    
    @discord.ui.button(label="Roblox", emoji="🎮", style=discord.ButtonStyle.success)
    async def roblox_button(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self.process_obfuscation(interaction, 'roblox')


# ==================== BOT EVENTS ====================
@bot.event
async def on_ready():
    """Bot ready event"""
    print(f"""
╔═══════════════════════════════════════════════════════════╗
║          LuaShield Discord Bot v2.1.0                     ║
╠═══════════════════════════════════════════════════════════╣
║  Logged in as: {bot.user.name:<40} ║
║  User ID: {bot.user.id:<45} ║
║  Servers: {len(bot.guilds):<46} ║
╠═══════════════════════════════════════════════════════════╣
║  Commands:                                                ║
║    /obfuscate    - Obfuscate Lua code                     ║
║    /obf          - Quick obfuscate (alias)                ║
║    /presets      - View available presets                 ║
║    /help         - Show help                              ║
║    !obf <code>   - Prefix command                         ║
╚═══════════════════════════════════════════════════════════╝
    """)
    
    # Sync slash commands
    try:
        synced = await bot.tree.sync()
        print(f"✅ Synced {len(synced)} slash commands")
    except Exception as e:
        print(f"❌ Failed to sync commands: {e}")
    
    # Set presence
    await bot.change_presence(
        activity=discord.Activity(
            type=discord.ActivityType.watching,
            name="/obfuscate | LuaShield"
        )
    )


@bot.event
async def on_message(message: discord.Message):
    """Handle messages with attachments"""
    if message.author.bot:
        return
    
    # Check for .lua attachments without command
    if message.attachments:
        lua_files = [a for a in message.attachments 
                    if a.filename.lower().endswith(('.lua', '.txt')) 
                    and not a.filename.lower().endswith('.luac')]
        
        if lua_files and not message.content.startswith(BOT_PREFIX):
            # Hint user about the command
            pass  # Don't auto-process, let user use command
    
    await bot.process_commands(message)


# ==================== SLASH COMMANDS ====================
@bot.tree.command(name="obfuscate", description="Obfuscate Lua code with LuaShield VM")
@app_commands.describe(
    preset="Obfuscation preset to use",
    file="Lua file to obfuscate (.lua or .txt)",
    code="Lua code to obfuscate (if no file)"
)
@app_commands.choices(preset=[
    app_commands.Choice(name="💡 Light - Fast, minimal", value="light"),
    app_commands.Choice(name="⚡ Medium - Balanced", value="medium"),
    app_commands.Choice(name="🔒 Heavy - Strong", value="heavy"),
    app_commands.Choice(name="🛡️ Maximum - Maximum protection", value="maximum"),
    app_commands.Choice(name="🎮 Roblox - Optimized for Roblox", value="roblox"),
])
async def slash_obfuscate(
    interaction: discord.Interaction,
    preset: str = "medium",
    file: discord.Attachment = None,
    code: str = None
):
    """Slash command to obfuscate Lua code"""
    await interaction.response.defer()
    
    source = None
    
    # Get source from file or code parameter
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
            "Please provide either a **file** or **code** to obfuscate.\n\n"
            "**Examples:**\n"
            "• `/obfuscate file:script.lua`\n"
            "• `/obfuscate code:print('Hello')`",
            error=True
        )
        await interaction.followup.send(embed=embed)
        return
    
    # Validate
    is_valid, error = validate_lua_source(source)
    if not is_valid:
        embed = create_embed("❌ Invalid Input", error, error=True)
        await interaction.followup.send(embed=embed)
        return
    
    # Obfuscate
    try:
        result = await obfuscate_code(source, preset)
        preset_info = PRESETS[preset]
        
        if result.success:
            stats = result.stats
            embed = create_embed(
                "✅ Obfuscation Complete!",
                f"**Preset:** {preset_info['name']}\n"
                f"**Original:** {format_bytes(stats.get('original_size', 0))}\n"
                f"**Output:** {format_bytes(stats.get('output_size', 0))}\n"
                f"**Ratio:** {stats.get('compression_ratio', 0)}%\n"
                f"**Time:** {stats.get('time_ms', 0)}ms\n"
                f"**Functions:** {stats.get('functions', 0)}\n"
                f"**Instructions:** {stats.get('instructions', 0)}",
                success=True
            )
            
            if result.warnings:
                embed.add_field(
                    name="⚠️ Warnings",
                    value="\n".join(f"• {w}" for w in result.warnings[:3]),
                    inline=False
                )
            
            output_bytes = result.output.encode('utf-8')
            output_file = discord.File(
                io.BytesIO(output_bytes),
                filename=f"obfuscated_{preset}_{interaction.id}.lua"
            )
            
            await interaction.followup.send(embed=embed, file=output_file)
        else:
            embed = create_embed(
                "❌ Obfuscation Failed",
                "```\n" + "\n".join(result.errors) + "\n```",
                error=True
            )
            await interaction.followup.send(embed=embed)
            
    except Exception as e:
        embed = create_embed(
            "❌ Error",
            f"```\n{str(e)}\n```",
            error=True
        )
        await interaction.followup.send(embed=embed)


@bot.tree.command(name="obf", description="Quick obfuscate with preset selection")
@app_commands.describe(file="Lua file to obfuscate")
async def slash_obf_quick(interaction: discord.Interaction, file: discord.Attachment = None):
    """Quick obfuscate with button selection"""
    await interaction.response.defer()
    
    source = None
    
    if file:
        content, error = await read_attachment(file)
        if error:
            embed = create_embed("❌ Error", error, error=True)
            await interaction.followup.send(embed=embed)
            return
        source = content
    else:
        embed = create_embed(
            "📤 Upload a File",
            "Please upload a `.lua` or `.txt` file to obfuscate.\n\n"
            "**Usage:** `/obf file:yourscript.lua`",
            error=True
        )
        await interaction.followup.send(embed=embed)
        return
    
    # Validate
    is_valid, error = validate_lua_source(source)
    if not is_valid:
        embed = create_embed("❌ Invalid Input", error, error=True)
        await interaction.followup.send(embed=embed)
        return
    
    # Show preset selection
    embed = create_embed(
        "🔧 Select Obfuscation Preset",
        f"**File:** `{file.filename}`\n"
        f"**Size:** {format_bytes(len(source))}\n\n"
        "Choose a preset below:"
    )
    
    embed.add_field(
        name="💡 Light",
        value="Fast, minimal protection",
        inline=True
    )
    embed.add_field(
        name="⚡ Medium",
        value="Balanced protection",
        inline=True
    )
    embed.add_field(
        name="🔒 Heavy",
        value="Strong protection",
        inline=True
    )
    embed.add_field(
        name="🛡️ Maximum",
        value="Maximum protection",
        inline=True
    )
    embed.add_field(
        name="🎮 Roblox",
        value="Optimized for Roblox",
        inline=True
    )
    
    view = QuickObfuscateView(source, interaction.user.id)
    await interaction.followup.send(embed=embed, view=view)


@bot.tree.command(name="presets", description="View available obfuscation presets")
async def slash_presets(interaction: discord.Interaction):
    """Show available presets"""
    embed = create_embed(
        "🔧 Obfuscation Presets",
        "Available presets for LuaShield VM Obfuscator:"
    )
    
    for key, preset in PRESETS.items():
        config = preset['config']
        features = []
        if config.get('encrypt_strings'):
            features.append("🔐 Encryption")
        if config.get('rename_variables'):
            features.append("🔤 Var Rename")
        if config.get('add_junk_code'):
            features.append("🧬 Junk Code")
        if config.get('add_anti_tamper'):
            features.append("🛡️ Anti-Tamper")
        if config.get('add_anti_dump'):
            features.append("🚫 Anti-Dump")
        
        embed.add_field(
            name=f"{preset['name']}",
            value=f"{preset['description']}\n" + " ".join(features),
            inline=False
        )
    
    await interaction.response.send_message(embed=embed)


@bot.tree.command(name="help", description="Show help information")
async def slash_help(interaction: discord.Interaction):
    """Show help information"""
    embed = create_embed(
        "🛡️ LuaShield VM Obfuscator",
        "Powerful Lua obfuscation with real VM protection"
    )
    
    embed.add_field(
        name="📌 Commands",
        value=(
            "`/obfuscate` - Obfuscate with specific preset\n"
            "`/obf` - Quick obfuscate with buttons\n"
            "`/presets` - View available presets\n"
            "`/help` - Show this help"
        ),
        inline=False
    )
    
    embed.add_field(
        name="📁 Supported Files",
        value=(
            "✅ `.lua` - Lua source files\n"
            "✅ `.txt` - Text files with Lua code\n"
            "❌ `.luac` - Bytecode NOT supported"
        ),
        inline=True
    )
    
    embed.add_field(
        name="✨ Features",
        value=(
            "• Real VM-based obfuscation\n"
            "• AES-256 / XOR encryption\n"
            "• Anti-tamper & anti-dump\n"
            "• Roblox executor compatible\n"
            "• Loadstring support"
        ),
        inline=True
    )
    
    embed.add_field(
        name="🔗 Links",
        value=(
            "[GitHub](https://github.com/luashield/vm-obfuscator)\n"
            "[Web Interface](https://luashield.onrender.com)"
        ),
        inline=True
    )
    
    await interaction.response.send_message(embed=embed)


# ==================== PREFIX COMMANDS ====================
@bot.command(name='obf', aliases=['obfuscate', 'protect', 'lua'])
async def prefix_obfuscate(ctx: commands.Context, preset: str = 'medium', *, code: str = None):
    """
    Prefix command to obfuscate Lua code
    
    Usage:
        !obf <preset> <code>
        !obf medium print("Hello")
        !obf (with attachment)
    """
    source = None
    
    # Check for attachments
    if ctx.message.attachments:
        attachment = ctx.message.attachments[0]
        content, error = await read_attachment(attachment)
        if error:
            embed = create_embed("❌ Error", error, error=True)
            await ctx.reply(embed=embed)
            return
        source = content
    elif code:
        # Handle case where preset might be part of code
        if preset not in PRESETS:
            code = f"{preset} {code}" if code else preset
            preset = 'medium'
        source = code
    else:
        embed = create_embed(
            "📋 Usage",
            "**With code:**\n"
            f"`{BOT_PREFIX}obf <preset> <code>`\n"
            f"`{BOT_PREFIX}obf medium print('Hello')`\n\n"
            "**With file:**\n"
            f"Upload a `.lua` file with `{BOT_PREFIX}obf <preset>`\n\n"
            "**Presets:** `light`, `medium`, `heavy`, `maximum`, `roblox`"
        )
        await ctx.reply(embed=embed)
        return
    
    # Validate preset
    if preset not in PRESETS:
        preset = 'medium'
    
    # Validate source
    is_valid, error = validate_lua_source(source)
    if not is_valid:
        embed = create_embed("❌ Invalid Input", error, error=True)
        await ctx.reply(embed=embed)
        return
    
    # Processing message
    preset_info = PRESETS[preset]
    processing_msg = await ctx.reply(
        embed=create_embed(
            "⏳ Processing...",
            f"Obfuscating with **{preset_info['name']}** preset..."
        )
    )
    
    try:
        result = await obfuscate_code(source, preset)
        
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
            
            output_bytes = result.output.encode('utf-8')
            output_file = discord.File(
                io.BytesIO(output_bytes),
                filename=f"obfuscated_{preset}_{ctx.message.id}.lua"
            )
            
            await processing_msg.edit(embed=embed, attachments=[output_file])
        else:
            embed = create_embed(
                "❌ Obfuscation Failed",
                "```\n" + "\n".join(result.errors) + "\n```",
                error=True
            )
            await processing_msg.edit(embed=embed)
            
    except Exception as e:
        embed = create_embed(
            "❌ Error",
            f"```\n{str(e)}\n```",
            error=True
        )
        await processing_msg.edit(embed=embed)


@bot.command(name='presets')
async def prefix_presets(ctx: commands.Context):
    """Show available presets (prefix command)"""
    embed = create_embed(
        "🔧 Obfuscation Presets",
        "Available presets:"
    )
    
    for key, preset in PRESETS.items():
        embed.add_field(
            name=f"`{key}` - {preset['name']}",
            value=preset['description'],
            inline=True
        )
    
    await ctx.reply(embed=embed)


@bot.command(name='help', aliases=['h', 'commands'])
async def prefix_help(ctx: commands.Context):
    """Show help (prefix command)"""
    embed = create_embed(
        "🛡️ LuaShield Bot Help",
        f"**Prefix:** `{BOT_PREFIX}`"
    )
    
    embed.add_field(
        name="Commands",
        value=(
            f"`{BOT_PREFIX}obf <preset> <code>` - Obfuscate code\n"
            f"`{BOT_PREFIX}obf` + attachment - Obfuscate file\n"
            f"`{BOT_PREFIX}presets` - View presets\n"
            f"`{BOT_PREFIX}help` - Show this help"
        ),
        inline=False
    )
    
    embed.add_field(
        name="Slash Commands",
        value=(
            "`/obfuscate` - Full obfuscation options\n"
            "`/obf` - Quick obfuscate with buttons\n"
            "`/presets` - View presets\n"
            "`/help` - Help"
        ),
        inline=False
    )
    
    await ctx.reply(embed=embed)


# ==================== ERROR HANDLERS ====================
@bot.event
async def on_command_error(ctx: commands.Context, error):
    """Handle command errors"""
    if isinstance(error, commands.CommandNotFound):
        return
    
    if isinstance(error, commands.MissingPermissions):
        embed = create_embed("❌ Permission Denied", str(error), error=True)
        await ctx.reply(embed=embed)
        return
    
    if isinstance(error, commands.CommandOnCooldown):
        embed = create_embed(
            "⏳ Cooldown",
            f"Please wait {error.retry_after:.1f}s before using this command again.",
            error=True
        )
        await ctx.reply(embed=embed)
        return
    
    # Log error
    print(f"Command error: {error}")
    embed = create_embed(
        "❌ Error",
        f"An error occurred: {str(error)}",
        error=True
    )
    await ctx.reply(embed=embed)


@bot.tree.error
async def on_app_command_error(interaction: discord.Interaction, error):
    """Handle slash command errors"""
    print(f"Slash command error: {error}")
    
    embed = create_embed(
        "❌ Error",
        f"An error occurred: {str(error)}",
        error=True
    )
    
    if interaction.response.is_done():
        await interaction.followup.send(embed=embed, ephemeral=True)
    else:
        await interaction.response.send_message(embed=embed, ephemeral=True)


# ==================== MAIN ====================
def run_bot():
    """Run the Discord bot"""
    if not BOT_TOKEN:
        print("❌ Error: DISCORD_BOT_TOKEN environment variable not set")
        print("Please set it in your environment or .env file:")
        print("  export DISCORD_BOT_TOKEN='your-token-here'")
        return
    
    try:
        bot.run(BOT_TOKEN)
    except discord.LoginFailure:
        print("❌ Error: Invalid bot token")
    except Exception as e:
        print(f"❌ Error: {e}")


if __name__ == '__main__':
    run_bot()
