#!/usr/bin/env python3
"""
LuaShield Unified Runner
Run web server, Discord bot, or both
"""

import os
import sys
import threading
import argparse
from dotenv import load_dotenv

# Load environment variables
load_dotenv()


def run_web():
    """Run Flask web server"""
    print("🌐 Starting Web Server...")
    from app import app
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port)


def run_bot():
    """Run Discord bot"""
    print("🤖 Starting Discord Bot...")
    from bot import run_bot as start_bot
    start_bot()


def run_both():
    """Run both web server and Discord bot"""
    print("🚀 Starting LuaShield (Web + Bot)...")
    
    # Run web server in a thread
    web_thread = threading.Thread(target=run_web, daemon=True)
    web_thread.start()
    
    # Run bot in main thread (Discord.py requires main thread)
    run_bot()


def main():
    parser = argparse.ArgumentParser(description='LuaShield Runner')
    parser.add_argument(
        'mode',
        nargs='?',
        choices=['web', 'bot', 'both'],
        default='both',
        help='Run mode: web (Flask), bot (Discord), or both (default)'
    )
    parser.add_argument(
        '--port',
        type=int,
        default=5000,
        help='Port for web server (default: 5000)'
    )
    
    args = parser.parse_args()
    os.environ['PORT'] = str(args.port)
    
    print("""
╔═══════════════════════════════════════════════════════════╗
║           LuaShield VM Obfuscator v2.1.0                  ║
║         Real VM-based Lua Obfuscation                     ║
╚═══════════════════════════════════════════════════════════╝
    """)
    
    if args.mode == 'web':
        run_web()
    elif args.mode == 'bot':
        run_bot()
    else:
        run_both()


if __name__ == '__main__':
    main()
