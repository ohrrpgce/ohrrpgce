import struct

class AST_node:

    def __init__(self, _type, children = None, leaf = None):
        self.type = _type
        self.children = children
        self.leaf = leaf

    def __repr__(self):
        "Doesn't print children; use AST_print for that."
        data = self.type
        if self.leaf != None:
            data += ": " + str(self.leaf)
        return data

class AST_call_signature:

    def __init__(self, _id, n_args):
        self.id = _id
        self.n_args = n_args

class AST_local_variable:

    def __init__(self, _id, value = None):
        self.id = _id
        self.value = value

class _AST_state:

    def __init__(self):

        self.root = None
        self.error = None
        self.last_error_lineno = None
        self.last_error_lexpos = None
        self.text = None
        self.initial_lineno = None

        # -- pass 1 -- global state

        # name: value
        self.triggers = {
            "script": 0,
            "plotscript": 1,
        }

        # name: number
        self.constants = {}

        # name: AST_call_signature
        self.functions = {}

        # name: id
        self.globals = {}

        # name: AST_call_signature
        self.scripts = {}
        self.scripts_last_id = 32767

        # -- pass 2 -- per script state

        # name: AST_local_variable
        self.locals = {}
        self.locals_last_id = None

        # string: id
        self.strings = None
        self.strings_table = None

    def build(self, lexer, yacc, text, lineno = None, name = None, debuglog = None):

        self.text = text
        self.script_name = name

        self.root = None
        self.error = None
        self.last_error_lineno = None
        self.last_error_lexpos = None

        if lineno:
            lexer.lineno = lineno  # Reset line number
        self.initial_lineno = lexer.lineno

        yacc.parse(text, tracking = True, debug = debuglog)

        if not self.root or self.error:
            return False

        return True

    def show_error_line(self, lexpos_or_span, lineno, caret = '^'):
        "Return a two-line string displaying a line of self.text"
        # lexposend is inclusive, but points to the beginning of a token, NOT the end of it!
        try:
            lexpos, lexposend = lexpos_or_span
        except:
            lexpos = lexpos_or_span
            lexposend = lexpos

        line_start = self.text.rfind('\n', 0, lexpos) + 1
        line_end = self.text.find('\n', lexpos)
        if line_end == -1:
            line_end = len(self.text)
        assert line_start <= lexpos <= line_end
        prefix = "Line %-4d " % lineno
        return (prefix + self.text[line_start : line_end] + "\n"
                + " " * (len(prefix) + lexpos - line_start) + caret * (lexposend - lexpos + 1) + "\n")

    def add_error(self, lexpos_or_span, lineno, message, caret = '^'):
        "Add an error, to print along with the source line, after parsing is done"
        # if lineno == self.last_error_lineno:
        #     # Hide multiple errors on a line, since following errors likely caused by the first
        #     return
        self.last_error_lineno = lineno
        if self.error is None:
            self.error = ""

        if isinstance(lexpos_or_span, int):
            lexpos_or_span = lexpos_or_span, lexpos_or_span
        if lexpos_or_span != self.last_error_lexpos:
            self.last_error_lexpos = lexpos_or_span

            self.error += "\n" + self.show_error_line(lexpos_or_span, lineno, caret)
        self.error += "%s\n" % (message)

    def eof(self):
        "Called when an EOF error occurs"
        if AST_state.error is None:
            AST_state.error = "continue"

    def __repr__(self):

        if self.error:
            return "AST_state: " + self.error

        if not self.root:
            return "AST_state: empty"

        return AST_print(self.root, 0).rstrip()

    def reset_locals(self):

        self.locals = {}
        self.locals_last_id = -1

        self.strings = {}
        self.strings_table = bytes()

        self.script_name = None

    def alloc_function(self, name, _id, n_args, args):

        sig = AST_call_signature(_id, n_args)
        sig.args = args
        self.functions[name] = sig

    def alloc_script(self, name, trigger, n_args, args):

        self.scripts_last_id -= 1

        sig = AST_call_signature(
            self.scripts_last_id, n_args
        )

        sig.trigger = trigger
        sig.args = []

        # get arguments default value

        for node in args:
            if node.children:
                sig.args.append(node.children[0])
            else:
                sig.args.append(AST_node("number", None, 0))

        self.scripts[name] = sig

    def alloc_local(self, name):

        self.locals_last_id += 1
        self.locals[name] = AST_local_variable(self.locals_last_id)

    def alloc_string(self, name):

        if name in self.strings:
            return self.strings[name]

        offset = len(self.strings_table) // 4
        self.strings[name] = offset

        data = bytes(name, "latin-1")
        self.strings_table += struct.pack("<I", len(data))

        for i in range(0, len(data), 4):
            self.strings_table += struct.pack("4s", data[i:])

        return offset

# singleton
AST_state = _AST_state()

def AST_print(node, indent):

    data = ("  " * indent) + str(node) + "\n"

    if node.children:
        for child in node.children:
            data += AST_print(child, indent + 1)

    return data
