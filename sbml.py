#Colin Roye 110378271
import sys
import ply.lex as lex
import ply.yacc as yacc
import readline
# mypl.py
# Tokenizer, Parser, Evaluator for a simple propositional logic

# List of token names


tokens = (
          'INTEGER', 'FLOAT',  'BOOLEAN', 'STRING', #DATA TYPES
          'R_PAREN', 'L_PAREN',
          'R_BRACK', 'L_BRACK',
          'COMMA',
          'MULT',
          'POW',
          'DIV',
          'DIVINT',
          'MOD',
          'PLUS',
          'MINUS',
          'IN',
          'CONS',
          'COLON',
          'NOT',
          'AND_ALSO',
          'OR_ELSE',
          'LTE',
          'GTE',
          'EQ_EQ',
          'NEQ',
          'LT',
          'GT',
          'EQ',
          'HASHTAG',
          'SEMICOLON'
          )


t_SEMICOLON = r';'
t_HASHTAG = r'\#'
t_L_PAREN = r'\('
t_R_PAREN = r'\)'
t_L_BRACK = r'\['
t_R_BRACK = r']'
t_COMMA = r','
t_POW = r'\*\*'
t_MULT= r'\*'
t_DIVINT = r'div'
t_DIV = r'/'
t_MOD = r'mod'
t_PLUS = r'\+'
t_MINUS = r'-'
t_IN = r'in'
t_CONS = r'::'
t_COLON = r':'
t_NOT = r'not'
t_AND_ALSO = r'andalso'
t_OR_ELSE = r'orelse'
t_LTE = r'<='
t_GTE = r'>='
t_EQ_EQ = r'=='
t_NEQ = r'<>'
t_LT = r'<'
t_GT = r'>'
t_EQ   = r'='

def t_BOOLEAN(t):
     r'True|False'
     t.value = BoolNode(t.value)
     return t


def t_FLOAT(t):
    r'(((\d)*\.(\d+)((e(-?)\d+)?)))'
    t.value = NumNode(t.value)
    return t

def t_INTEGER(t):
    r'(\d+)'
    t.value = NumNode(t.value)
    return t

def t_STRING(t):
     r'(\"[^\"]*\")|(\'[^\']*\')'
     t.value = StrNode(t.value[1:-1])
     return t

# def t_VARIABLE(t):
#     r'[a-z]\d{0,2}'
#     t.value = t.value
#     return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
#
def t_error(t):
    if t.value[0] != ' ':
        t.lexer.skip(len(t.value))
    else:
        t.lexer.skip(1)

lexer = lex.lex()
def reduce(expr):
    if expr != None and hasattr(expr, 'nodeType'):
        while(hasattr(expr, 'nodeType')):
            expr = expr.eval()
        return expr
    else:
        return expr

class BinOpNode():
    def __init__(self, e1, op, e2):
        self.e1 = e1
        self.e2 = e2
        self.op = op
        self.nodeType = "binop"
    def eval(self):
        e1 = reduce(self.e1)
        e2 = reduce(self.e2)
        if isinstance(e1, bool) or isinstance(e2, bool):
            return None
        if (isinstance(e1, int) or isinstance(e1, float)) and (isinstance(e2, int) or isinstance(e2, float)):
            if self.op == '**':
                return e1 ** e2
            if self.op == '*':
                return e1 * e2
            if self.op == 'div':
                return e1 // e2
            if self.op == '/':
                return e1 / e2
            if self.op == 'mod':
                return e1 % e2
            if self.op == '+':
                return e1 + e2
            if self.op == '-':
                return e1 - e2
        if isinstance(e1, str) and isinstance(e2, str):
            if self.op == '+':
                return e1 + e2
        if isinstance(e1, list) and isinstance(e2, list):
            if self.op == '+':
                return e1 + e2


class ListOpNode():
    def __init__(self, var, op, l):
        self.var = var
        self.l = l
        self.op = op
        self.nodeType = "listop"
    def eval(self):
        var = reduce(self.var)
        l = reduce(self.l)
        if(isinstance(l, list)):
            if(self.op == 'in'):
                return var in l
            if(self.op == '::'):
                return [var]+l

class BooleanOpNode():
    def __init__(self, e1, op, e2):
        self.e1 = e1
        self.e2 = e2
        self.op = op
        self.nodeType = "boolop"

    def eval(self):
        e1 = reduce(self.e1)
        e2 = reduce(self.e2)
        if self.op == '==':
            return e1 == e2
        if self.op == '<>':
            return e1 != e2
        if self.op == '<=':
            return e1 <= e2
        if self.op == '>=':
            return e1 >= e2
        if self.op == '>':
            return e1 > e2
        if self.op == '<':
            return e1 < e2
        if(isinstance((e1), bool) and isinstance((e2), bool)):
            if self.op == 'andalso':
                return e1 and e2
            if self.op == 'orelse':
                return e1 or e2


class IndexNode():
    def __init__(self, list, ind):
        self.l =  list
        self.ind = ind
        self.nodeType = "ind"

    def eval(self):
        rawList = reduce(reduce(self.l))
        rawIndex = reduce(self.ind)
        if(isinstance(rawIndex,int)):
            if(isinstance(rawList, list) or isinstance(rawList, str)):
                if(rawIndex<len(rawList) and rawIndex >= 0):
                    return rawList[rawIndex]
            if(isinstance(rawList, tuple)):
                if(rawIndex<=len(rawList) and rawIndex > 0):
                    return rawList[rawIndex-1]




class NumNode():
    def __init__(self, val):
        self.val = val
        self.nodeType = "num"
    def eval(self):
        if '.' in self.val:
            return float(self.val)
        else:
            return int(self.val)

class StrNode():
    def __init__(self, val):
        self.val = val
        self.nodeType = "str"
    def eval(self):
        return self.val

class BoolNode():
    def __init__(self, val):
        self.val = (val == 'True');
        self.nodeType = "bool"
    def eval(self):
        return self.val

class ListNode():
    def __init__(self, val):
        self.val = val
        self.nodeType = "list"
    def eval(self):
        return list(map(lambda x:reduce(x),self.val))

class TupleNode():
    def __init__(self, val):
        self.val = val
        self.nodeType = "tuple"
    def eval(self):
        return tuple(map(lambda x:reduce(x),self.val))


def p_line(t):
    """line : expr SEMICOLON"""
    ret = t[1].eval()

    if(ret != None):
        t[0] = t[1].eval()
    else:
        print("SEMANTIC ERROR")

def p_expr(t):
    """expr : INTEGER
            | FLOAT
            | STRING
            | BOOLEAN
            | list
            | tuple"""
    t[0] = (t[1])

def p_tuple_index(t):
    """expr : HASHTAG INTEGER expr %prec HASHTAG"""
    t[0] = IndexNode(t[3], t[2])


def p_tuple(t):
    """tuple : L_PAREN sequence R_PAREN
             | L_PAREN R_PAREN"""
    if len(t) == 3:
        t[0] = TupleNode(())
    else:
        t[0] = TupleNode(t[2])

def p_binop_expr(t):
    """expr : expr POW expr
            | expr MULT expr
            | expr DIVINT expr
            | expr DIV expr
            | expr MOD expr
            | expr PLUS expr
            | expr MINUS expr"""
    t[0] = BinOpNode(t[1],t[2],t[3])

def p_expr_uminus(t):
     'expr : MINUS expr %prec UMINUS'
     t[0] = BinOpNode(-reduce(t[2]),'+',0)

def p_boolop(t):
    """expr : expr AND_ALSO expr
            | expr OR_ELSE expr
            | expr LTE expr
            | expr GTE expr
            | expr EQ_EQ expr
            | expr NEQ expr
            | expr LT expr
            | expr GT expr"""
    t[0] = BooleanOpNode(t[1],t[2],t[3])

def p_listop(t):
    """expr : expr IN expr
            | expr CONS expr"""
    t[0] = ListOpNode(t[1],t[2],t[3])

# def p_negationOp(t):
#     """expr : NOT expr ? prec U"""
#     if(isinstance(reduce(t[2]), bool)):
#         t[0] = BooleanOpNode(BoolNode(not reduce(t[2])), 'AND', 'TRUE')
def p_negationOp(t):
     'expr : NOT expr %prec UMINUS'
     t[0] = BooleanOpNode(not reduce(t[2]),'andalso',BoolNode('True'))

def p_list_index(t):
    """expr : expr L_BRACK expr R_BRACK"""
    t[0] = IndexNode(t[1], t[3])


def p_list(t): #err
    """list : L_BRACK sequence R_BRACK
            | L_BRACK R_BRACK"""
    if len(t) == 3:
        t[0] = ListNode([])
    else:
        t[0] = ListNode(t[2]);




def p_sequence(t):
       """sequence : expr COMMA sequence
                   | expr"""
       if len(t) == 4:
           t[0] =  [t[1]] + t[3]
       else:
           t[0] = [t[1]]




def p_parenthesized(t):
    """expr : L_PAREN expr R_PAREN"""
    t[0] = t[2]




def p_error(t):
    print("SYNTAX ERROR")



precedence = (
        ('left','AND_ALSO','OR_ELSE'),
        ('left','EQ_EQ','NEQ','LT','LTE','GT','GTE'),
        ('right','CONS'),
        ('left','IN'),
        ('left','MINUS','PLUS'),
        ('left','MULT','DIV','DIVINT','MOD'),
        ('right','UMINUS'),
        ('right','POW'),
        ('left', 'L_BRACK','R_BRACK'),
        ('right','HASHTAG'),
        ('left','L_PAREN','R_PAREN')

)

parser = yacc.yacc()
#

filepath = sys.argv[1];

deb = False
if('-d' in sys.argv):
    sys.argv.remove('-d')
    deb = True
e = False
if('-e' in sys.argv):
    sys.argv.remove('-e')
    e = True
try:
    if('-l' in sys.argv):
        sys.argv.remove('-l')
        result = parser.parse(sys.argv[1], debug=deb)
        if(result != None):
            print(result)
    elif ('-r' in sys.argv[1]):
        sys.argv.remove('-r')
        while True:
            try:
                s = input("Enter a proposition: ")
            except EOFError:
                break
            if not s:
                continue
            result = parser.parse(s, debug = deb)
            if result != None:
                print("RESULT:", result)

    else:
        with open(filepath) as fp:
           line = fp.readline()
           while line:
               result = parser.parse(line, debug=deb)
               if result != None:
                   print(result)
               line = fp.readline()
except Exception as err:
    print("ERROR")
    if(e):
        print(err)
    pass
