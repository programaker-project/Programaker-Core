#!/usr/bin/env python3

from flask import Flask, request
import json
import db

application = app = Flask(__name__)

db.init()

@app.route('/api/bot_orders/set', methods=['POST'])
def set_bot_orders():
    commands = request.json['commands']
    token = request.json['token'].strip()
    db.register_commands_for_token(token, commands)

    return json.dumps({"success": True})

def main():
    app.run(port=8080, debug=True)

if __name__ == '__main__':
    main()
