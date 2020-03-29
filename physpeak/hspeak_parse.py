# AST_node and AST_state are given to this module by hspeak_ast.py
AST_node = None
AST_state = None

# Lexing rules

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
}

tokens = [
    'NAME', 'BINARY', 'NUMBER', 'STRING',
    'ASSIGN', 'PLUS_EQUAL', 'MINUS_EQUAL',
    'LT_EQUAL', 'GT_EQUAL',
    'BOOL_AND', 'BOOL_OR',
    'BITWISE_AND', 'BITWISE_OR',
    'EQUAL_EQUAL', 'MINUS_MINUS', 'MORE_LESS',
    'BOOL_XOR', 'BITWISE_XOR',
    'REMINDER', 'HEX',
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
t_REMINDER = r'(?i),\s*MOD\s*,'

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
    print("Illegal character '%s' at line %d" % (t.value[0], t.lineno))
    t.lexer.skip(1)

t_ignore = ' \t'

# Build the lexer
import ply.lex as lex
lex.lex()

# Parsing rules

precedence = (
    ('left', '=', 'ASSIGN', 'PLUS_EQUAL', 'MINUS_EQUAL', ),
    ('left', 'BOOL_OR', ),
    ('left', 'BOOL_XOR', ),
    ('left', 'BOOL_AND', ),
    ('left', '|', 'BITWISE_OR', ),
    ('left', 'BITWISE_XOR', ),
    ('left', '&', 'BITWISE_AND', ),
    ('left', 'EQUAL_EQUAL', 'MORE_LESS', ),
    ('left', '<', 'LESS_THAN', 'LT_EQUAL', '>', 'GREATER_THAN', 'GT_EQUAL', ),
    ('left', '+', '-', 'MINUS_MINUS', ),
    ('left', '*', '/', '%', 'REMINDER', ),
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
                    | expression_list ',' empty
    """
    p[0] = p[1] + [p[3]]

def p_expr_list_2(p):
    """
    expression_list : expression
                    | void
                    | empty
    """
    p[0] = [p[1]]

def p_expr_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

def p_expr_block(p):
    "block : '(' expression_list ')'"
    p[0] = p[2]

def p_default_value(p):
    "void : reference '=' expression"
    p[0] = AST_node("value", [p[3]], p[1].leaf)

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

def p_else_1(p):
    "flow_else : ELSEIF condition flow_then flow_else"
    p[0] = AST_node("flow", p[2:], "if")

def p_else_2(p):
    "flow_else : ELSEIF condition flow_then"
    p[0] = AST_node("flow", p[2:], "if")

def p_else_3(p):
    "flow_else : ELSE block"
    p[0] = AST_node("flow", p[2], "else")

def p_then(p):
    "flow_then : THEN block"
    p[0] = AST_node("flow", p[2], "then")

def p_do_1(p):
    "void : DO block"
    p[0] = AST_node("flow", p[2], p[1])

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
               | expression REMINDER expression
    """
    p[0] = AST_node('binop', [p[1], p[3]], p[2])

def p_unop(p):
    "expression : '-' expression %prec UMINUS"
    try:
        p[0] = AST_node('number', None, -int(p[2].leaf))
    except:
        p[0] = AST_node('unop', [p[2]], p[1])

def p_name_concat_1(p):
    """
    name_concat : name_concat IF
                | name_concat THEN
                | name_concat ELSE
                | name_concat FOR
                | name_concat WHILE
                | name_concat DO
                | name_concat BREAK
                | name_concat CONTINUE
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

def p_variable(p):
    "expression : name_concat"
    p[0] = AST_node("value", None, p[1])

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
    "expression : '@' reference"
    p[0] = AST_node("reference", None, p[2].leaf)

def p_reference(p):
    "reference : name_concat"
    p[0] = AST_node("reference", None, p[1])

def p_empty(p):
    "empty : "
    p[0] = AST_node('empty')

def p_string_ref_1(p):
    "string_ref : reference"
    p[0] = AST_node("string_ref_1", None, p[1].leaf)

def p_string_ref_2(p):
    "string_ref : NUMBER"
    p[0] = AST_node("string_ref_2", None, int(p[1]))

def p_string_val(p):
    "string_val : STRING"
    p[0] = AST_node('string_val', None, p[1])

def p_string_binop(p):
    """
    void : '$' string_ref '=' string_val
         | '$' string_ref '+' string_val
    """
    p[0] = AST_node("string_op", [p[2], p[4]], p[1] + p[3])

def p_error(p):
    if p:
        AST_state.error = "Syntax error at '%s'" % (p.value)
    else:
        # the error is recoverable
        AST_state.error = "continue"

# Build the parser
import ply.yacc as yacc
yacc.yacc()
