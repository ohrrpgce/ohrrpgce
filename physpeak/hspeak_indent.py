#!/usr/bin/env python3

from html import escape

reserved = {
    'else if': 'ELSE_IF',
    'case': 'CASE',
    'plotscript': 'PLOTSCRIPT',
    'exit': 'EXIT',
    'for': 'FOR',
    'switch': 'SWITCH',
    'while': 'WHILE',
    'do': 'DO',
    'else': 'ELSE',
    'end': 'END',
    'then': 'THEN',
    'script': 'SCRIPT',
    'if': 'IF',
}

tokens = [
    'LESS_EQUAL', 'PLUS_EQUAL', 'MINUS_MINUS',
    'ASSIGN', 'NUMBER', 'STRING_PLUS',
    'EQUAL_EQUAL', 'BOOL_AND', 'MINUS_EQUAL',
    'EXP_EXP', 'STRING_EQUAL', 'LESS_MORE',
    'SHIFT_LEFT', 'NAME', 'MORE_EQUAL',
    'BOOL_OR', 'COMMENT', 'SHIFT_RIGHT',
    'STRING',
] + list(reserved.values())

literals = (
    '+', '-', '*', '/',
    '<', '>', '$', '^', '@',
    ',', '(', ')', '=', ':',
    '.', '?', '%',
)

# Tokens

t_PLUS_EQUAL = r'\+='
t_MINUS_EQUAL = r'-='
t_STRING_PLUS = r'\$\+'
t_STRING_EQUAL = r'\$='
t_MINUS_MINUS = r'--'
t_EXP_EXP = r'\^\^'
t_EQUAL_EQUAL = r'=='
t_LESS_MORE = r'<>'
t_SHIFT_RIGHT = r'>>'
t_SHIFT_LEFT = r'<<'
t_LESS_EQUAL = r'<='
t_MORE_EQUAL = r'>='
t_ASSIGN = r':='
t_BOOL_AND = r'&&'
t_BOOL_OR = r'\|\|'
t_NUMBER = r'-?\d+'
t_STRING = r'\"([^\\\n]|(\\.))*?\"'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_ ]*'
    t.value = t.value.rstrip()
    t.type = reserved.get(t.value, 'NAME')
    return t

def t_COMMENT(t):
    r'\#.*\n'
    t.value = t.value.rstrip()
    t.lexer.lineno += 1
    return t

def t_newline(t):
    r'\n'
    t.lexer.lineno += 1

def t_error(t):
    print("Illegal character '%s' at line %d" % (t.value[0], t.lineno))
    t.lexer.skip(1)

t_ignore = ' \t'

# Build the lexer
import ply.lex as lex
lex.lex()

tok_space_before = [
    '>', 'THEN', 'BOOL_AND', 'WHILE', 'SHIFT_LEFT',
    'LESS_MORE', 'EQUAL_EQUAL', 'IF', 'LESS_EQUAL',
    'PLUS_EQUAL', '-', 'FOR', 'EXIT', 'ELSE_IF',
    'MINUS_EQUAL', 'MORE_EQUAL', 'STRING_PLUS',
    'ELSE', '=', 'ASSIGN', 'MINUS_MINUS', '<',
    'SHIFT_RIGHT', 'BOOL_OR', 'CASE', '+',
    'DO', '*', '/', 'END', 'COMMENT', 'SWITCH',
    '^', '%',
]

tok_space_after = tok_space_before + [',']

tok_gray = (
    'SWITCH', '-', 'DO', 'MINUS_EQUAL', 'FOR', 'CASE',
    'BOOL_OR', '<', 'STRING_PLUS', '$', '/', 'ELSE',
    'MORE_EQUAL', 'PLUS_EQUAL', 'ASSIGN', 'BOOL_AND',
    '>', 'COMMENT', '@', 'SHIFT_RIGHT', '*',
    'SHIFT_LEFT', 'LESS_EQUAL', 'LESS_MORE', 'THEN',
    'ELSE_IF', 'EQUAL_EQUAL', '+', 'MINUS_MINUS',
    'IF', '=', 'WHILE', ':', '^',
)

tok_red = (
    'NAME', '.', '?',
)

tok_blue = (
    'NUMBER',
)

tok_green = (
    '(', ')', ',', 'PLOTSCRIPT', 'SCRIPT', 'EXIT', 'END',
)

tok_black = (
    'STRING',
)

if __name__ == "__main__":

    fdi = open("plotscr.hsd", "r")
    data = fdi.read()

    fdo = open("test.html", "w")

    # Give the lexer some input
    lex.input(data)

    fdo.write("<html><body><pre>")

    last_type = ""
    last_lineno = 0

    indent = 0
    in_color = False

    # Tokenize
    while True:

        tok = lex.token()
        if not tok:
            break

        if tok.type == ")" or \
           tok.type == "END":
            indent -= 1

        indent = max(indent, 0)

        if tok.lineno != last_lineno:

            if in_color:
                fdo.write('</font>')
                in_color = False

            fdo.write('\n%5d | ' % (tok.lineno))
            fdo.write(" " * 4 * indent)

            last_type = ""

        if tok.type == "(" or \
           tok.type == "PLOTSCRIPT" or \
           tok.type == "SCRIPT":
            indent += 1

        if tok.type in tok_space_before and last_type != "" or \
           last_type == "NAME" and tok.type == "NAME":
            fdo.write(" ")

        if tok.type != last_type:

            if in_color:
                fdo.write('</font>')

            if tok.type in tok_gray:
                fdo.write('<font color="gray">')
            elif tok.type in tok_red:
                fdo.write('<font color="red">')
            elif tok.type in tok_blue:
                fdo.write('<font color="blue">')
            elif tok.type in tok_green:
                fdo.write('<font color="green">')
            elif tok.type in tok_black:
                fdo.write('<font color="black">')
            else:
                fdo.write('<font color="black">{%s}' % (tok.type))

            in_color = True

        fdo.write(escape(tok.value))

        if tok.type in tok_space_after:
            fdo.write(" ")

        last_type = tok.type
        last_lineno = tok.lineno

    fdo.write("\n</pre></body></html>\n")
    fdo.close()

    fdi.close()
