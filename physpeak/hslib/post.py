import operator

from .ast import AST_node, AST_state
from .gen import kind_and_id, KIND_NUMBER


def hsdiv(a, b):
    """HS / operator (same as / in FB or C)"""
    if a * b < 0 and a % b:
        return a // b + 1
    else:
        return a // b

def hsmod(a, b):
    """HS ,mod, operator (same as MOD in FB or % in C):
     a  b  a,mod,b  Python a%b:
    -1  5    -1       4
     1 -5     1      -4
    -1 -5    -1      -1
    """
    ret = a % b
    if ret and a * b < 0:
        return ret - b
    else:
        return ret

def overflow_int32(a):
    ret = a % 0x1_0000_0000
    if ret >= 0x8000_0000:
        return ret - 0x1_0000_0000
    return ret

unary_operators = {
    "negate": operator.neg,
}

binary_operators = {
    "^": operator.pow,
    "*": operator.mul,
    "/": hsdiv,
    "%": hsmod,
    "+": operator.add,
    "-": operator.sub,
}
# TODO: support other spellings like ,mod,
# TODO: many other operators missing

def AST_post():

    # constant folding
    AST_post_1(AST_state.root)

    # patch flow calls
    AST_post_2(AST_state.root)

    # default arguments for calls
    AST_post_3(AST_state.root)

def AST_post_1(node):

    if not node.children:
        return

    for child in node.children:
        AST_post_1(child)

    for child in node.children:

        if child.type not in ("binop", "unop"):
            continue

        if child.leaf in unary_operators:
            k1, v1 = kind_and_id(child.children[0])
            if k1 == KIND_NUMBER:
                child.type = "number"
                child.leaf = overflow_int32(unary_operators[child.leaf](v1))
                child.children = None
            continue

        if child.leaf in binary_operators:
            k1, v1 = kind_and_id(child.children[0])
            k2, v2 = kind_and_id(child.children[1])
            if k1 == KIND_NUMBER and k2 == KIND_NUMBER:
                try:
                    result = overflow_int32(binary_operators[child.leaf](v1, v2))
                    child.type = "number"
                    child.leaf = result
                    child.children = None
                except ZeroDivisionError:
                    print("Division by zero: found the expression (or equivalent) %s %s %s" % (v1, child.left, v2))
            continue

def AST_post_2(node):

    if not node.children:
        return

    for child in node.children:
        AST_post_2(child)

    for child in node.children:

        if child.type not in ("function", "value"):
            continue

        if child.leaf in ("break", "continue"):
            if not child.children:
                child.children = [AST_node("number", None, 1)]
            continue

def AST_post_3(node):

    if not node.children:
        return

    for child in node.children:
        AST_post_3(child)

    for child in node.children:

        if child.type not in ("function", "value"):
            continue

        if child.leaf in AST_state.scripts:
            p_func = AST_state.scripts[child.leaf]
        elif child.leaf in AST_state.functions:
            p_func = AST_state.functions[child.leaf]
        else:
            continue

        # variable number of arguments
        if p_func.n_args < 0:
            continue

        p_args = p_func.args.copy()

        while len(p_args) < p_func.n_args:
            p_args.append(AST_node("number", None, 0))

        if child.children:
            for i in range(min(len(p_args), len(child.children))):
                p_args[i] = child.children[i]

        if p_args:
            child.children = p_args
        else:
            child.children = None
