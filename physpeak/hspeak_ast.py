#!/usr/bin/env python3

# run this module for a REPL

# external modules should do:
# from hspeak_ast import AST_state
# and use the methods in it

import struct

class AST_node:
    def __init__(self, _type, _children = None, _leaf = None):
        self.type = _type
        self.children = _children
        self.leaf = _leaf

class AST_call_signature:
    pass

class _AST_state:

    def __init__(self):

        self.root = None
        self.error = None
        self.last_error_lineno = None
        self.text = None
        self.initial_lineno = None

        # -- pass 1 -- global state

        # name: value
        self.triggers = {
            "script": 0,
            "plotscript": 1,
        }

        # name: value
        self._constants = {}

        # name: AST_signature
        self.functions = {}

        # name: id
        self.globals = {}

        # name: AST_signature
        self.scripts = {}
        self.scripts_last_id = 32767

        # -- pass 2 -- per script state

        self.script_name = None

        # name: id
        self.locals = None
        self.locals_last_id = None

        # string: id
        self.strings = None
        self.strings_table = None

    def build(self, text, lineno = None, _name = None, debuglog = None):

        self.text = text
        self.script_name = _name

        self.root = None
        self.error = None
        self.last_error_lineno = None

        if lineno:
            hspeak_parse.lexer.lineno = lineno  # Reset line number
        self.initial_lineno = hspeak_parse.lexer.lineno

        hspeak_parse.yacc.parse(text, tracking = True, debug = debuglog)

        if not self.root or self.error:
            return False

        return True

    def show_error_line(self, lexpos, lineno):
        "Return a two-line string displaying a line of self.text"
        line_start = self.text.rfind('\n', 0, lexpos) + 1
        line_end = self.text.find('\n', lexpos)
        if line_end == -1:
            line_end = len(self.text)
        assert line_start <= lexpos <= line_end
        return (self.text[line_start : line_end] + "\n"
                + " " * (lexpos - line_start) + "^\n")

    def add_error(self, lexpos, lineno, message):
        if lineno == self.last_error_lineno:
            # Hide multiple errors on a line, since following errors likely caused by the first
            return
        self.last_error_lineno = lineno
        if self.error is None:
            self.error = ""
        self.error += "\n" + self.show_error_line(lexpos, lineno)
        self.error += "Line %d: %s\n" % (lineno, message)

    def eof(self):
        "Called when an EOF error occurs"
        if AST_state.error is None:
            AST_state.error = "continue"

    def reset_locals(self):

        self.locals = {}
        self.locals_last_id = -1

        self.strings = {}
        self.strings_table = bytes()

        self.script_name = None

    def alloc_function(self, name, _id, n_args, _args):

        sig = AST_call_signature()

        sig.id = _id
        sig.n_args = n_args
        sig.args = _args

        self.functions[name] = sig

    def alloc_script(self, name, trigger, n_args, _args):

        self.scripts_last_id -= 1

        sig = AST_call_signature()

        sig.id = self.scripts_last_id
        sig.trigger = trigger
        sig.n_args = n_args
        sig.args = []

        for node in _args:
            if node.children:
                sig.args.append(node.children[0])
            else:
                sig.args.append(AST_node("number", None, 0))

        self.scripts[name] = sig

    def alloc_local(self, name):

        self.locals_last_id += 1
        self.locals[name] = \
            self.locals_last_id

    def alloc_string(self, name):

        if name in self.strings:
            return self.strings[name]

        offset = int(len(self.strings_table) / 4)
        self.strings[name] = offset

        _str = bytes(name, "latin-1")
        self.strings_table += struct.pack("<I", len(_str))

        for i in range(0, len(_str), 4):
            self.strings_table += struct.pack("4s", _str[i:])

        return offset

    def print(self):
        AST_print(self.root, 0)

# singleton
AST_state = _AST_state()

# --

def AST_print(node, indent):

    s = str(node.type)
    if node.leaf != None:
        s += ": " + str(node.leaf)

    print(("  " * indent) + s)

    if node.children:
        for child in node.children:
            AST_print(child, indent + 1)

def AST_input(s):

    try:
        r = raw_input(s)
    except:
        r = input(s)

    return r

# --

import hspeak_parse
hspeak_parse.AST_node = AST_node
hspeak_parse.AST_state = AST_state

# --

if __name__ == "__main__":

    # Use readline with a history
    try:
        import readline  # Not available in all Python builds
        histfile = ".hspeak_history"
        try:
            readline.read_history_file(histfile)
            readline.set_history_length(100)
        except IOError:
            pass
        import atexit
        atexit.register(readline.write_history_file, histfile)
    except ImportError:
        pass

    import logging
    log = None

    AST_state.reset_locals()

    while True:

        # get a line of text and try to parse it

        try:
            s1 = AST_input('HSpeak> ')
        except EOFError:
            print()
            break

        if not s1:
            continue

        if s1.startswith("@debug"):
            if "help" in s1:
                print("Print yacc debug message. Usage: @debug [error|info|full|off|help] (default to info)")
                continue
            if not log:
                logging.basicConfig()
                log = logging.getLogger()
            if "error" in s1:
                log.setLevel(logging.ERROR)
            elif "full" in s1:
                log.setLevel(logging.DEBUG)
            elif "off" in s1:
                log = None
                print("Logging disabled")
                continue
            else: #if "info" in s1:
                log.setLevel(logging.INFO)
            print("Logging level set to", logging.getLevelName(log.level))
            continue

        rv = AST_state.build(s1, 1, debuglog = log)

        # if the parser reported an error at the end of the line
        # then add another line and see if things improve

        while AST_state.error == "continue":

            try:
                s2 = AST_input(' .... > ')
            except EOFError:
                print()
                break

            if not s2:
                break

            # a newline implictily adds a ','
            s1 += ',\n' + s2

            rv = AST_state.build(s1, debuglog = log)

        if rv:
            AST_state.print()
        else:
            print(AST_state.error)
