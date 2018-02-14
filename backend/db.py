import os
import sqlite3
import json
import base64

DB_FILE = 'db.sqlite3'

def register_commands_for_token(token, commands):
    db = sqlite3.connect(DB_FILE)
    cur = db.cursor()

    cur.execute('''
    INSERT OR REPLACE INTO chatbots (token, commands, state)
    VALUES(?, ?, null);
    ''', (token, commands))

    db.commit()
    db.close()

def init():
    db = sqlite3.connect(DB_FILE)
    cur = db.cursor()
    cur.execute('''
    CREATE TABLE IF NOT EXISTS chatbots (
        token TEXT,
        commands TEXT,
        state TEXT,
        PRIMARY KEY(token)
    );
    ''')

    db.commit()
    db.close()
