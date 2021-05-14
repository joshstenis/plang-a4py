# ------------------------------------------------------------
# Programming Assignment #4 Python Rewrite
# ---------------------------------------------
# Author: Josh Stenis
# ------------------------------------------------------------

tokens = (
    'ID', 'INTEGER', 'FLOAT', 'DT_INT', 'DT_FLOAT', 'LITERAL_STR', 
    'ASSIGN', 'ADD', 'SUB', 'MUL', 'DIV', 'LT', 'GT', 'LEQ', 'GEQ', 
    'READ', 'WRITE', 'BEGIN', 'END', 'REPEAT', 'UNTIL', 'DO', 'WHILE', 'IF', 'THEN', 'ELSE', 
    'SEMICOLON', 'COMMA', 'LBRACK', 'RBRACK', 'LPAR', 'RPAR'
)

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t
def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t
def t_ASSIGN(t):
    r':='
    return t
def t_LEQ(t):
    r'<='
    return t
def t_GEQ(t):
    r'>='
    return t
def t_LT(t):
    r'<'
    return t
def t_GT(t):
    r'>'
    return t
def t_ADD(t):
    r'\+'
    return t
def t_SUB(t):
    r'-'
    return t
def t_MUL(t):
    r'\*'
    return t
def t_DIV(t):
    r'/'
    return t
def t_REPEAT(t):
    r'repeat'
    return t
def t_UNTIL(t):
    r'until'
    return t
def t_WHILE(t):
    r'while'
    return t
def t_BEGIN(t):
    r'begin'
    return t
def t_DT_FLOAT(t):
    r'float'
    return t
def t_WRITE(t):
    r'write'
    return t
def t_ELSE(t):
    r'else'
    return t
def t_READ(t):
    r'read'
    return t
def t_THEN(t):
    r'then'
    return t
def t_END(t):
    r'end'
    return t
def t_DT_INT(t):
    r'int'
    return t
def t_DO(t):
    r'do'
    return t
def t_IF(t):
    r'if'
    return t
def t_LITERAL_STR(t):
    r'\".\"'
    return t
def t_SEMICOLON(t):
    r';'
    return t
def t_COMMA(t):
    r','
    return t
def t_LPAR(t):
    r'\('
    return t
def t_RPAR(t):
    r'\)'
    return t
def t_LBRACK(t):
    r'\['
    return t
def t_RBRACK(t):
    r']'
    return t
def t_ID(t):
    r'[a-zA-Z_]\w*'
    return t

t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print('Illegal charater: {}'.format(t.value))

import ply.lex as lex
lexer = lex.lex()


# ---------------------------------------------
# Grammar rules (.y) below
# ---------------------------------------------

env = {}
stack = []

def p_program(p):
    '''program : stmt_list SEMICOLON'''

def p_stmt_list(p):
    '''stmt_list : stmt_list SEMICOLON stmt
                 | stmt'''

def p_stmt(p):
    '''stmt : assignment
            | read
            | write
            | declaration
            | construct_while
            | construct_repeat
            | construct_if
            | block'''

def p_block(p):
    '''block : BEGIN stmt_list END'''
    p[0] = p[2]

def p_construct_while(p):
    '''construct_while : WHILE LPAR l_expr RPAR DO block'''
    while(p[3]):
        p[6]

def p_construct_repeat(p):
    '''construct_repeat : REPEAT stmt_list UNTIL LPAR l_expr RPAR'''

def p_construct_if(p):
    '''construct_if : IF LPAR l_expr RPAR stmt construct_else'''
    if p[3]:
        p[5]
    else:
        if p[6] is None: pass
        else:
            p[6]

def p_construct_else(p):
    '''construct_else : 
                      | ELSE stmt'''
    try:
       p[0] = p[2]
    except IndexError: p[0] = None

def p_l_expr(p):
    '''l_expr : a_expr oprel a_expr'''
    print('l_expr...')
    if p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '<=':
        p[0] = p[1] <= p[3]
    elif p[2] == '>=':
        p[0] = p[1] >= p[3]

def p_oprel(p):
    '''oprel : LT
             | GT
             | LEQ
             | GEQ'''
    p[0] = p[1]

def p_assignment(p):
    '''assignment : ID arr_idx ASSIGN a_expr
                  | ID ASSIGN a_expr'''
    if p[2] == ':=':
        env[p[1]] = p[3]
    else:
        env[p[1]][p[2]] = p[4]

def p_declaration(p):
    '''declaration : datatype ID arr_size'''
    if p[1] == 'int':
        env[p[2]] = [int(0)] * p[3]
    elif p[1] == 'float':
        env[p[2]] = [float(0)] * p[3]

def p_arr_size(p):
    '''arr_size : LBRACK a_expr RBRACK
                | '''
    try:
        p[0] = p[2]
    except IndexError:
        p[0] = 1

def p_datatype(p):
    '''datatype : DT_INT
                | DT_FLOAT'''
    p[0] = p[1]

def p_a_expr(p):
    '''a_expr : a_expr a_op a_expr
              | varref
              | INTEGER
              | FLOAT
              | LPAR a_expr RPAR
              | SUB a_expr
              | LITERAL_STR'''
    try:
        if p[2] == '*':
            p[0] = p[1] * p[3]
        elif p[2] == '/':
            p[0] = p[1] / p[3]
        elif p[2] == '+':
            p[0] = p[1] + p[3]
        elif p[2] == '-':
            p[0] = p[1] - p[3]
    except:
        if p[1] == '(':
            p[0] = p[1]
        elif p[1] == '-':
            p[0] = p[2] * -1
        else: p[0] = p[1]

def p_a_op(p):
    '''a_op : ADD
            | SUB
            | MUL
            | DIV'''
    p[0] = p[1]

def p_varref(p):
    '''varref : ID arr_idx'''
    p[0] = env[p[1]][p[2]]

def p_arr_idx(p):
    '''arr_idx : LBRACK a_expr RBRACK'''
    p[0] = p[2]

def p_read(p):
    '''read : READ varlist'''

def p_write(p):
    '''write : WRITE expr_list'''

def p_varlist(p):
    '''varlist : varlist COMMA varref
               | varref'''

def p_expr_list(p):
    '''expr_list : expr_list COMMA a_expr
                 | a_expr'''

def p_error(p):
    print('Parsing error: "{0}" at line {1}'.format(p, p.lexer.lineno))

import ply.yacc as yacc
parser = yacc.yacc()


# ---------------------------------------------
# Run parser
# ---------------------------------------------

prgm = open('inputs-outputs/{}.smp'.format(input('File: ')), 'r').read()
parser.parse(prgm, lexer)
print('Environment: {}'.format(env))