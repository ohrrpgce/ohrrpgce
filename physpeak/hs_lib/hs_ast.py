import struct

class AST_node:

    def __init__(self, _type, _children = None, _leaf = None):
        self.type = _type
        self.children = _children
        self.leaf = _leaf

    def __repr__(self):
        return '<"%s", %s, "%s">' % (self.type, self.children, self.leaf)

class AST_call_signature:

    def __init__(self, _id, _n_args):
        self.id = _id
        self.n_args = _n_args

class AST_local_variable:

    def __init__(self, _id, _value = None):
        self.id = _id
        self.value = _value

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

        # name: number
        self._constants = {}

        # name: AST_call_signature
        self.functions = {}

        # name: id
        self.globals = {}

        # name: AST_call_signature
        self.scripts = {}
        self.scripts_last_id = 32767

        # -- pass 2 -- per script state

        # name: AST_local_variable
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
            hs_parse.lexer.lineno = lineno  # Reset line number
        self.initial_lineno = hs_parse.lexer.lineno

        hs_parse.yacc.parse(text, tracking = True, debug = debuglog)

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

        sig = AST_call_signature(_id, n_args)
        sig.args = _args
        self.functions[name] = sig

    def alloc_script(self, name, trigger, n_args, _args):

        self.scripts_last_id -= 1

        sig = AST_call_signature(
            self.scripts_last_id, n_args
        )

        sig.trigger = trigger
        sig.args = []

        # get arguments default value

        for node in _args:
            if node.children:
                sig.args.append(node.children[0])
            else:
                sig.args.append(AST_node("number", None, 0))

        self.scripts[name] = sig

    def alloc_local(self, name):

        self.locals_last_id += 1
        self.locals[name] = \
            AST_local_variable(self.locals_last_id)

    def alloc_string(self, name):

        if name in self.strings:
            return self.strings[name]

        offset = len(self.strings_table) // 4
        self.strings[name] = offset

        _str = bytes(name, "latin-1")
        self.strings_table += struct.pack("<I", len(_str))

        for i in range(0, len(_str), 4):
            self.strings_table += struct.pack("4s", _str[i:])

        return offset

    def _print(self):
        AST_print(self.root, 0)

# singleton
AST_state = _AST_state()

def AST_print(node, indent):

    s = str(node.type)
    if node.leaf != None:
        s += ": " + str(node.leaf)

    print(("  " * indent) + s)

    if node.children:
        for child in node.children:
            AST_print(child, indent + 1)

# --

import hs_parse
hs_parse.AST_node = AST_node
hs_parse.AST_state = AST_state
