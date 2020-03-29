import array
import struct

from hspeak_ast import AST_state

KIND_NUMBER = 1 # int32. ID is the value
KIND_FLOW = 2 # Flow control (flow_table), including begin and end.
KIND_GLOBAL = 3 # Global variable
KIND_LOCAL = 4 # Local variable
KIND_MATH = 5 # Math builtin function
KIND_FUNCTION = 6 # Builtin command
KIND_SCRIPT = 7 # Call to script
KIND_NONLOCAL = 8 # Nonlocal variable

binop_table = {
    "random": 0,
    "exponent": 1, "^": 1,
    "modulus": 2, "%": 2, ",MOD,": 2,
    "divide": 3, "/": 3,
    "multiply": 4, "*": 4,
    "subtract": 5, "--": 5, "-": 5,
    "add": 6, "+": 6,
    "xor": 7, ",XOR,": 7,
    "or": 8, "|": 8, ",OR,": 8,
    "and": 9, "&": 9, ",AND,": 9,
    "equal": 10, "==": 10,
    "notequal": 11, "<>": 11,
    "lessthan": 12, "<": 12, "<<": 12,
    "greaterthan": 13, ">": 13, ">>": 13,
    "lessthanorequalto": 14, "<=": 14,
    "greaterthanorequalto": 15, ">=": 15,
    "setvariable": 16, ":=": 16,
    "increment": 17, "+=": 17,
    "decrement": 18, "-=": 18,
    "logand": 20, "&&": 20,
    "logor": 21, "||": 21,
    "logxor": 22, "^^": 22,
}

unop_table = {
    "not": 19, "-": 19,
    "abs": 23,
    "sign": 24,
    "sqrt": 25,
}

flow_table = {
    "do": 0,
    "begin": 1,
    "end": 2,
    "return": 3,
    "if": 4,
    "then": 5,
    "else": 6,
    "for": 7,
    "while": 10,
    "break": 11,
    "continue": 12,
    "exitscript": 13,
    "exitreturning": 14,
    "switch": 15,
}

string_op_table = {
    "$=": 251, # setstringfromtable
    "$+": 252, # appendstringfromtable
}

def kind_and_id(node):
    "Returns a pair"

    if node.type == 'number':

        return KIND_NUMBER, node.leaf

    if node.type == 'function':

        if node.leaf in AST_state.scripts:
            return KIND_SCRIPT, AST_state.scripts[node.leaf].id
        if node.leaf in AST_state.functions:
            return KIND_FUNCTION, AST_state.functions[node.leaf].id

        # compatibility
        if node.leaf in binop_table:
            return KIND_MATH, binop_table[node.leaf]
        if node.leaf in unop_table:
            return KIND_MATH, unop_table[node.leaf]
        if node.leaf in flow_table:
            return KIND_FLOW, flow_table[node.leaf]

    if node.type == 'binop':

        canonical = node.leaf.upper().replace(' ', '')
        if canonical in binop_table:
            return KIND_MATH, binop_table[canonical]

    if node.type == 'unop':

        if node.leaf in unop_table:
            return KIND_MATH, unop_table[node.leaf]

    if node.type == "reference":

        if node.leaf in AST_state.locals:
            return KIND_NUMBER, -1 - AST_state.locals[node.leaf]
        if node.leaf in AST_state.globals:
            return KIND_NUMBER, AST_state.globals[node.leaf]
        if node.leaf in AST_state.scripts:
            return KIND_NUMBER, AST_state.scripts[node.leaf].id

    if node.type == "value":

        if node.leaf in AST_state.locals:
            return KIND_LOCAL, AST_state.locals[node.leaf]
        if node.leaf in AST_state.globals:
            return KIND_GLOBAL, AST_state.globals[node.leaf]
        if node.leaf in AST_state.scripts:
            return KIND_SCRIPT, AST_state.scripts[node.leaf].id
        if node.leaf in AST_state.functions:
            return KIND_FUNCTION, AST_state.functions[node.leaf].id
        if node.leaf in AST_state._constants:
            return KIND_NUMBER, AST_state._constants[node.leaf]

        # compatibility
        if node.leaf in flow_table:
            return KIND_FLOW, flow_table[node.leaf]

    if node.type == "flow":

        return KIND_FLOW, flow_table[node.leaf]

    if node.type == "string_op":

        if node.leaf in string_op_table:
            return KIND_FUNCTION, string_op_table[node.leaf]

    if node.type == "string_ref_1":

        if node.leaf in AST_state._constants:
            return KIND_NUMBER, AST_state._constants[node.leaf]

    if node.type == "string_ref_2":

        return KIND_NUMBER, node.leaf

    if node.type == "string_val":

        return KIND_NUMBER, AST_state.alloc_string(node.leaf)

    # if nothing matches, emit a placeholder
    print("unknown", node.type, node.leaf)
    return 0, 0

def compile_recurse(node, cmddata):

    kind, _id = kind_and_id(node)

    cmddata.extend((kind, _id))

    if kind in (KIND_FLOW, KIND_MATH, KIND_FUNCTION, KIND_SCRIPT):
        if node.children:
            cmddata.append(len(node.children))
        else:
            cmddata.append(0)

    if not node.children:
        return

    # Then append the offsets to the children. We don't know them
    # yet, add placeholders instead.
    childptrs = len(cmddata)
    for _ in node.children:
        cmddata.append(0)

    for idx, child in enumerate(node.children):
        # Now we know where the child will go
        # (because we don't bother to compress the output)
        cmddata[childptrs + idx] = len(cmddata)
        compile_recurse(child, cmddata)

def toHSZ():
    "Return a compiled .hsz lump as a bytes array"

    num_locals = len(AST_state.locals)
    num_args = AST_state.scripts[AST_state.script_name].n_args
    num_nonlocals = 0
    parent_script = 0
    nesting_depth = 0

    CODE_START_BYTE_OFFSET = 18
    HSZ_FORMAT_VERSION = 3

    cmddata = array.array('i')
    compile_recurse(AST_state.root, cmddata)
    cmddata = cmddata.tobytes()

    str_table = AST_state.strings_table
    str_table_offset = 0

    if len(str_table):
        str_table_offset = CODE_START_BYTE_OFFSET + len(cmddata)

    header = struct.pack('<4HI3H',
        CODE_START_BYTE_OFFSET,
        num_locals,
        num_args,
        HSZ_FORMAT_VERSION,
        str_table_offset,
        parent_script,
        nesting_depth,
        num_nonlocals
    )

    data = header + cmddata + str_table

    return data
