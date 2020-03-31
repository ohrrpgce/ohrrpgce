
from hs_parser_utils import describe_parser_expectation, tell_error

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
    'BINARY', 'HEX', 'NUMBER',
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
    '.', '?', '#', '&', '|',
    '%',
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
operator_list = "- * / + MINUS_MINUS LESS_THAN GREATER_THAN < LT_EQUAL > GT_EQUAL EQUAL_EQUAL MORE_LESS & BITWISE_AND | BITWISE_OR BOOL_AND BOOL_OR ^ BITWISE_XOR BOOL_XOR % REMAINDER".split()

t_ignore_COMMENT = r'\#.*'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_ ]*'
    t.value = t.value.replace(' ', '')
    t.type = reserved.get(t.value, 'NAME')
    return t

def t_BINARY(t):
    r'0b[01][01 ]*'
    t.value = int(t.value[2:].replace(' ', ''), 2)
    return t

def t_HEX(t):
    r'0x[0-9a-f][0-9a-f ]*'
    t.value = int(t.value[2:].replace(' ', ''), 16)
    return t

def t_NUMBER(t):
    r'[0-9][0-9 ]*'
    t.value = int(t.value.replace(' ', ''))
    return t

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = t.value[1:-1]
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
    ('left', '&', 'BITWISE_AND', ),
    ('left', 'EQUAL_EQUAL', 'MORE_LESS', ),
    ('left', '<', 'LESS_THAN', 'LT_EQUAL', '>', 'GREATER_THAN', 'GT_EQUAL', ),
    ('left', '+', '-', 'MINUS_MINUS', ),
    ('left', '*', '/', '%', 'REMAINDER', ),
    ('left', '^', ),
    ('right', 'UMINUS', ),
)

def p_statement_expr(p):
    "statement : expression_list"
    AST_state.root = AST_node("flow", p[1], "do")

def p_expr_list_1(p):
    """
    expression_list : expression_list ',' expression
                    | expression_list ',' void
    """
    p[0] = p[1] + [p[3]]

def p_expr_list_2(p):
    "expression_list : expression_list ',' empty"
    p[0] = p[1]

def p_expr_list_3(p):
    """
    expression_list : expression
                    | void
    """
    p[0] = [p[1]]

def p_expr_list_4(p):
    "expression_list : empty"
    p[0] = []

def p_expr_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

symdesc['block'] = "bracketed block of statements '(...)'"

def p_expr_block(p):
    "block : '(' expression_list ')'"
    p[0] = p[2]

def p_default_value(p):
    "void : name_concat '=' expression"
    p[0] = AST_node("value", [p[3]], p[1])

def p_define(p):
    "empty : VARIABLE block"
    p[0] = AST_node("empty")
    for arg in p[2]:
        AST_state.alloc_local(arg.leaf)

symdesc['void'] = "statement"

def p_assign(p):
    """
    void : reference ASSIGN expression
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
    case_list : case_list expression_list
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
    case_else_list : case_list CASE '(' ELSE ')' expression_list
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
    "expression : reference block"
    p[0] = AST_node("function", p[2], p[1].leaf)

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
               | expression '&' expression
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

symdesc['name_concat'] = "identifier"

def p_name_concat_1(p):
    """
    name_concat : name_concat IF
                | name_concat THEN
                | name_concat ELSEIF
                | name_concat ELSE
                | name_concat FOR
                | name_concat WHILE
                | name_concat DO
                | name_concat BREAK
                | name_concat CONTINUE
                | name_concat SWITCH
                | name_concat CASE
                | name_concat ':'
                | name_concat '.'
    """
    p[0] = p[1] + p[2]

def p_name_concat_2(p):
    "name_concat : name_concat NUMBER"
    p[0] = p[1] + str(p[2])

def p_name_concat_3(p):
    "name_concat : name_concat NAME"
    p[0] = p[1] + p[2].lower()

def p_name_concat_4(p):
    "name_concat : NAME"
    p[0] = p[1].lower()

def p_number(p):
    """
    expression : NUMBER
               | BINARY
               | HEX
    """
    p[0] = AST_node('number', None, p[1])

def p_condition(p):
    "condition : '(' expression ')'"
    p[0] = p[2]

def p_pointer(p):
    "expression : '@' name_concat"
    p[0] = AST_node("reference", None, p[2])

def p_reference(p):
    "reference : name_concat"
    p[0] = AST_node("reference", None, p[1])

def p_empty(p):
    "empty : "
    p[0] = AST_node('empty')

def p_string_ref_1(p):
    "string_ref : name_concat"
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
    "void : '$' string_ref '=' string_val"
    p[0] = AST_node("function", [p[2], p[4]], "setstringfromtable")

def p_string_op_2(p):
    "void : '$' string_ref '+' string_val"
    p[0] = AST_node("function", [p[2], p[4]], "appendstringfromtable")

def p_error(p):
    if p:
        msg = describe_parser_expectation(parser)
        if msg:
            msg = ": " + msg
        AST_state.add_error(p.lexpos, p.lineno, "Syntax error at '%s'%s" % (p.value, msg))
    else:
        # The error is recoverable
        AST_state.eof()

# Build the parser
import ply.yacc as yacc
parser = yacc.yacc()
