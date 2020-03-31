
from hs_parser_utils import describe_parser_expectation, tell_error, inclusive_span

# AST_node and AST_state are given to this module by hs_ast.py
AST_node = None
AST_state = None

# Human-readable description of symbols used when reporting errors
symdesc = {}


##############################################################################
#                               Lexing rules


reserved = {
    'if': 'IF',
    'then': 'THEN',
    'elseif': 'ELSEIF',
    'else': 'ELSE',
    'for': 'FOR',
    'while': 'WHILE',
    'do': 'DO',
    'break': 'BREAK',
    'continue': 'CONTINUE',
    'return': 'RETURN',
    'switch': 'SWITCH',
    'case': 'CASE',
    'variable': 'VARIABLE',
}

tokens = [
    'NAME', 'STRING',
    'NUMBER',
    'ASSIGN', 'PLUS_EQUAL', 'MINUS_EQUAL',
    'LT_EQUAL', 'GT_EQUAL',
    'BOOL_AND', 'BOOL_OR',
    'BITWISE_AND', 'BITWISE_OR',
    'EQUAL_EQUAL', 'MINUS_MINUS', 'MORE_LESS',
    'BOOL_XOR', 'BITWISE_XOR',
    'REMAINDER',
    'LESS_THAN', 'GREATER_THAN',
] + list(reserved.values())

literals = (
    '+', '-', '*', '/', '^',
    '<', '>', '$', '^', '@',
    ',', '(', ')', '=', ':',
    '.', '#', '|', '%',
)

# Tokens

t_ASSIGN = r':='
t_PLUS_EQUAL = r'\+='
t_MINUS_EQUAL = r'-='
t_LT_EQUAL = r'<='
t_GT_EQUAL = r'>='
t_BITWISE_AND = r'(?i),\s*AND\s*,'
t_BITWISE_OR = r'(?i),\s*OR\s*,'
t_BOOL_AND = r'&&'
t_BOOL_OR = r'\|\|'
t_EQUAL_EQUAL = r'=='
t_MINUS_MINUS = r'--'
t_MORE_LESS = r'<>'
t_LESS_THAN = r'<<'
t_GREATER_THAN = r'>>'
t_BITWISE_XOR = r'(?i),\s*XOR\s*,'
t_BOOL_XOR = r'\^\^'
t_REMAINDER = r'(?i),\s*MOD\s*,'

# This list of operators is used only while printing syntax errors.
# It should match the list of operators valid in an expression
operator_list = "- * / + MINUS_MINUS LESS_THAN GREATER_THAN < LT_EQUAL > GT_EQUAL EQUAL_EQUAL MORE_LESS BITWISE_AND | BITWISE_OR BOOL_AND BOOL_OR ^ BITWISE_XOR BOOL_XOR % REMAINDER".split()

t_ignore_COMMENT = r'\#.*'

def t_NAME(t):
    # \w also matches 0-9 and _, so we reverse \W instead for the first letter
    # Grab :'s unless they are part of := and &'s except for &&
    r"[^\W0-9]([\w0-9_'~?! ]|:(?!=)|&(?!&))*"
    t.original = t.value
    t.value = t.value.replace(' ', '').lower()
    t.type = reserved.get(t.value, 'NAME')
    return t

numeric_bases = {'x': 16, 'X': 16, 'o': 8, 'O': 8, 'b': 2, 'B': 2}

def t_NUMBER(t):
    r'(0x[0-9a-f][0-9a-f ]*|(0[ob])?[0-9][0-9 ]*)(?P<unit>[a-z:]+)?'

    # Check for trailing garbage (in future this would be used for units,
    # currently it's not necessary)
    unitgroup = t.lexer.lexmatch.group('unit')
    if unitgroup:
        span = inclusive_span(t.lexer.lexmatch.span('unit'))
        AST_state.add_error(span, t.lineno, "Garbage following a number; expected a separator like ',' or an operator to follow it.")
        t.value = t.value.replace(unitgroup, '')

    value = t.value.replace(' ', '')
    # Can't use int( ,base=0), it doesn't accept '01'
    base = 10
    if len(value) > 2:
        base = numeric_bases.get(value[1], 10)

    try:
        t.value = int(value, base)
    except Exception as ex:
        span = inclusive_span(t.lexer.lexmatch.span())
        AST_state.add_error(span, t.lineno, "Malformed number: %s" % ex)
        t.value = 0

    return t

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = t.value[1:-1]
    return t

def t_bad_string(t):
    r'\"([^\\\n]|(\\.))*'
    span = t.lexpos, t.lexer.lexpos - 1
    AST_state.add_error(span, t.lineno, 'String missing closing "')
    t.type = 'STRING'
    t.value = t.value[1:]
    return t

def t_newline(t):
    r'\n'
    t.lexer.lineno += 1

def t_error(t):
    AST_state.add_error(t.lexpos, t.lineno, "Illegal character '%s'" % (t.value[0],))
    t.lexer.skip(1)

t_ignore = ' \t'

# Build the lexer
import ply.lex as lex
lexer = lex.lex() #optimize = True)


##############################################################################
#                               Parsing rules


precedence = (
    ('left', 'ASSIGN', 'PLUS_EQUAL', 'MINUS_EQUAL', ),
    ('left', 'BOOL_OR', ),
    ('left', 'BOOL_XOR', ),
    ('left', 'BOOL_AND', ),
    ('left', '|', 'BITWISE_OR', ),
    ('left', 'BITWISE_XOR', ),
    ('left', 'BITWISE_AND', ),
    ('left', 'EQUAL_EQUAL', 'MORE_LESS', ),
    ('left', '<', 'LESS_THAN', 'LT_EQUAL', '>', 'GREATER_THAN', 'GT_EQUAL', ),
    ('left', '+', '-', 'MINUS_MINUS', ),
    ('left', '*', '/', '%', 'REMAINDER', ),
    ('left', '^', ),
    ('right', 'UMINUS', ),
)

def p_start(p):
    "start : statement_list"
    AST_state.root = AST_node("flow", p[1], "do")

def p_statement(p):
    """
    statement : expression
              | void
    """
    p[0] = [p[1]]

def p_statement_2(p):
    "statement : variable"
    p[0] = []

symdesc['nonempty_statement_list'] = "one or more statements"

def p_ne_statement_list(p):
    "nonempty_statement_list : statement"
    p[0] = p[1]

def p_ne_statement_list_2(p):
    "nonempty_statement_list : nonempty_statement_list statement"
    p[0] = p[1] + p[2]

def p_ne_statement_list_3(p):
    "nonempty_statement_list : nonempty_statement_list ',' statement"
    p[0] = p[1] + p[3]

symdesc['statement_list'] = "a list of statements"

def p_empty(p):
    "empty : "
    p[0] = []

def p_statement_list(p):
    """
    statement_list : empty
                   | nonempty_statement_list
    """
    p[0] = p[1]

def p_expr_list_0(p):
    "expression_list : expression"
    p[0] = [p[1]]

def p_expr_list_1(p):
    "expression_list : expression_list ',' expression"
    p[0] = p[1] + [p[3]]

def p_expr_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

symdesc['block'] = "bracketed block of statements '(...)'"

def p_expr_block(p):
    "block : '(' statement_list ')'"
    p[0] = p[2]

def p_default_value(p):
    "void : NAME '=' expression"
    p[0] = AST_node("value", [p[3]], p[1])

symdesc['name_list'] = "list of variable names"

def p_name_list_0(p):
    "name_list : NAME"
    p[0] = [p[1]]

def p_name_list_1(p):
    "name_list : name_list ',' NAME"
    p[0] = p[1] + [p[3]]

def p_define(p):
    "variable : VARIABLE '(' name_list ')'"
    for varname in p[3]:
        AST_state.alloc_local(varname)

symdesc['void'] = "statement"

def p_assign(p):
    """
    expression : reference ASSIGN expression
               | reference PLUS_EQUAL expression
               | reference MINUS_EQUAL expression
    """
    p[0] = AST_node("binop", [p[1], p[3]], p[2])

def p_if_1(p):
    "void : IF condition flow_then flow_else"
    p[0] = AST_node("flow", p[2:], "if")

def p_if_2(p):
    "void : IF condition flow_then"
    p[0] = AST_node("flow", p[2:], "if")

symdesc['flow_else'] = "else() block or elseif()..."

def p_else_1(p):
    "flow_else : ELSEIF condition flow_then flow_else"
    p[0] = AST_node("flow", p[2:], "if")

def p_else_2(p):
    "flow_else : ELSEIF condition flow_then"
    p[0] = AST_node("flow", p[2:], "if")

def p_else_3(p):
    "flow_else : ELSE block"
    p[0] = AST_node("flow", p[2], "else")

symdesc['flow_then'] = "then() block"

def p_then(p):
    "flow_then : THEN block"
    p[0] = AST_node("flow", p[2], "then")

def p_do_1(p):
    "void : DO block"
    p[0] = AST_node("flow", p[2], p[1])

symdesc['flow_do'] = "do() block"

def p_do_2(p):
    "flow_do : DO block"
    p[0] = AST_node("flow", p[2], p[1])

def p_for_1(p):
    "void : FOR '(' reference ',' expression ',' expression ')' flow_do"
    p[0] = AST_node("flow", [p[3], p[5], p[7], AST_node("number", None, 1), p[9]], p[1])

def p_for_2(p):
    "void : FOR '(' reference ',' expression ',' expression  ',' expression ')' flow_do"
    p[0] = AST_node("flow", [p[3], p[5], p[7], p[9], p[11]], p[1])

def p_while(p):
    "void : WHILE condition flow_do"
    p[0] = AST_node("flow", p[2:], p[1])

## The actions for the SWITCH/CASE grammar directly builds the AST in the
## (rather unintuitive) HSZ format, instead of building an AST that
## reflects the syntax and then post-processing it later.

def p_case_list_0(p):
    """
    case_list : CASE '(' expression_list ')'
    """
    p[0] = p[3]

def p_case_list_01(p):
    """
    case_list : ',' CASE '(' expression_list ')'
    """
    p[0] = p[4]

# Each expression is added as a child of 'switch'
def p_case_list_1(p):
    """
    case_list : case_list CASE '(' expression_list ')'
    """
    p[0] = p[1] + p[4]

# Expressions not in a 'case' get packed into a do()
def p_case_list_2(p):
    """
    case_list : case_list nonempty_statement_list
    """
    p[0] = p[1] + [AST_node("flow", p[2], "do")]

# Kludge mostly for newlines
def p_case_list_3(p):
    """
    case_list : case_list ','
    """
    p[0] = p[1]

## The last arg to 'switch' is a do() which is the else() case

symdesc['case_else_list'] = "case list"

def p_finalised_case_list1(p):
    """
    case_else_list : case_list
    """
    p[0] = p[1] + [AST_node("flow", [], "do")]

def p_finalised_case_list2(p):
    """
    case_else_list : case_list ELSE block
    """
    p[0] = p[1] + [AST_node("flow", p[3], "do")]

def p_finalised_case_list3(p):
    """
    case_else_list : case_list CASE '(' ELSE ')' statement_list
    """
    p[0] = p[1] + [AST_node("flow", p[6], "do")]

def p_switch(p):
    "void : SWITCH condition DO '(' case_else_list ')'"
    p[0] = AST_node("flow", [p[2]] + p[5], p[1])

def p_flow_1(p):
    """
    void : BREAK
         | CONTINUE
         | RETURN
    """
    p[0] = AST_node('flow', None, p[1])

def p_flow_2(p):
    """
    void : BREAK condition
         | CONTINUE condition
         | RETURN condition
    """
    p[0] = AST_node("flow", [p[2]], p[1])

def p_function(p):
    "expression : reference '(' expression_list ')'"
    p[0] = AST_node("function", p[3], p[1].leaf)

def p_function_2(p):
    "expression : reference '(' ')'"
    p[0] = AST_node("function", [], p[1].leaf)

# Either a function call or a variable
def p_value(p):
    "expression : reference"
    p[0] = AST_node("value", None, p[1].leaf)

def p_binop(p):
    """
    expression : expression '*' expression
               | expression '/' expression
               | expression '+' expression
               | expression MINUS_MINUS expression
               | expression '-' expression
               | expression LESS_THAN expression
               | expression GREATER_THAN expression
               | expression '<' expression
               | expression LT_EQUAL expression
               | expression '>' expression
               | expression GT_EQUAL expression
               | expression EQUAL_EQUAL expression
               | expression MORE_LESS expression
               | expression BITWISE_AND expression
               | expression '|' expression
               | expression BITWISE_OR expression
               | expression BOOL_AND expression
               | expression BOOL_OR expression
               | expression '^' expression
               | expression BITWISE_XOR expression
               | expression BOOL_XOR expression
               | expression '%' expression
               | expression REMAINDER expression
    """
    p[0] = AST_node('binop', [p[1], p[3]], p[2])

def p_unop(p):
    "expression : '-' expression %prec UMINUS"
    if p[2].type == "number":
        p[0] = AST_node('number', None, -p[2].leaf)
    else:
        p[0] = AST_node('unop', [p[2]], p[1])

def p_number(p):
    """
    expression : NUMBER
    """
    p[0] = AST_node('number', None, p[1])

def p_condition(p):
    "condition : '(' expression ')'"
    p[0] = p[2]

def p_pointer(p):
    "expression : '@' NAME"
    p[0] = AST_node("reference", None, p[2])

def p_reference(p):
    "reference : NAME"
    p[0] = AST_node("reference", None, p[1])

def p_string_ref_1(p):
    "string_ref : NAME"
    p[0] = AST_node('value', None, p[1])

def p_string_ref_2(p):
    "string_ref : NUMBER"
    p[0] = AST_node('number', None, p[1])

def p_string_ref_3(p):
    "string_ref : condition"
    p[0] = p[1]

def p_string_val(p):
    "string_val : STRING"
    p[0] = AST_node("string_val", None, p[1])

def p_string_op_1(p):
    "expression : '$' string_ref '=' string_val"
    p[0] = AST_node("function", [p[2], p[4]], "setstringfromtable")

def p_string_op_2(p):
    "expression : '$' string_ref '+' string_val"
    p[0] = AST_node("function", [p[2], p[4]], "appendstringfromtable")


##############################################################################
#                          Error messages & recovery


def p_error(p):
    if p:
        msg = describe_parser_expectation(parser)
        if msg:
            msg = ": " + msg
        AST_state.add_error(p.lexpos, p.lineno, "Syntax error at '%s'%s" % (p.value, msg))
    else:
        # The error is recoverable
        AST_state.eof()

def p_assign_err(p):
    """
    void : expression ASSIGN
         | expression PLUS_EQUAL
         | expression MINUS_EQUAL
    """
    tell_error(p, 2, "The left-hand-side of :=/+=/-= must be a variable name, not an expression.")
    raise SyntaxError

def p_expr_string_err(p):
    "expression : error STRING"
    tell_error(p, 2, """Strings can't be used as expressions; they can only appear as part of $...="..." or $...+"...".""")

def p_expr_err(p):
    "expression_list : expression error ','"
    tell_error(p, 0, "Error while parsing an expression.")

def p_condition_err(p):
    "condition : '(' error ')'"
    tell_error(p, 0, "Condition should be a (single) expression.")

# def p_condition_err2(p):
#     "condition : '(' expression_list ')'"
#     tell_error(p, 2, "xCondition should be a (single) expression.")
#     raise SyntaxError

def p_expr_block_err(p):
    "block : '(' error ')'"
    tell_error(p, 0, "Block doesn't contain valid list of statements")


##############################################################################

# Build the parser
import ply.yacc as yacc
parser = yacc.yacc()
