import sys
import ply.lex as lex
import ply.yacc as yacc

# mypl.py
# Tokenizer, Parser, Evaluator for a simple propositional logic

# List of token names


tokens = (
          'INTEGER', 'FLOAT', 'BOOL', 'STRING', #DATA TYPES
          'R_PAREN', 'L_PAREN',
          'R_BRACK', 'L_BRACK',
          'COMMA',
         # 'F_SLASH',
          'MULT',
          'POW',
          'DIV',
          'MOD',
          'PLUS',
    #      'UMINUS',
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
          'BOOLEAN',
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
t_DIV = r'div'
t_MOD = r'mod'
t_PLUS = r'\+'
#t_UMINUS = r'-'
t_MINUS = r'-'
t_IN = r'in'
# t_CONS = r'::'
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
t_BOOLEAN = r'True|False'


def t_FLOAT(t):
    #(-?)
    r'(((\d)*\.(\d+)((e(-?)\d+)?)))'
    t.value = NumNode(t.value)
    return t
def t_INTEGER(t):
    #(-?)
    r'(\d+)'
    t.value = NumNode(t.value)
    return t


def t_STRING(t):
     r'(\"[^\"]*\")|(\'[^\']*\')'
     t.value = Node(t.value[1:-1])
     return t

# def t_VARIABLE(t):
#     r'[a-z]\d{0,2}'
#     t.value = t.value
#     return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("SYNTAX ERROR: %s at %d" % (t.value[0], t.lexer.lineno))
    t.lexer.skip(1)

lexer = lex.lex()
def reduce(expr):
    #print("EXPR:", expr)

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

        if (isinstance(e1, int) or isinstance(e1, float)) and (isinstance(e2, int) or isinstance(e2, float)):#self.e1.nodeType == 'num' and self.e1.nodeType == 'num':
            if self.op == '**':
                return e1 ** e2
            if self.op == '*':
                return e1 * e2
            if self.op == '/':
                return e1 / e2
            if self.op == '%':
                return e1 % e2
            if self.op == '+':
                if isinstance(e1+e2, type(None)):
                    print("ADD IS NONE")
                return e1 + e2
            if self.op == '-':
                return e1 - e2
        if isinstance(e1, str) and isinstance(e2, str):
            if self.op == '+':
                return e1 + e2
        else:
            return 22
class BooleanOpNode():
    def __init__(self, e1, op, e2):
        self.e1 = e1
        self.e2 = e2
        self.op = op
        self.nodeType = "boolop"

    def eval(self):
        e1 = reduce(self.e1)
        e2 = reduce(self.e2)
        print("OP:", self.op)
        if self.op == '==':
            return e1 == e2
        if self.op == '!=':
            return e1 != e2
        if self.op == 'andalso':
            return e1 and e2
        if self.op == 'orelse':
            return e1 or e2
        if self.op == '<=':
            return e1 <= e2
        if self.op == '>=':
            return e1 >= e2
        if self.op == '>':
            return e1 > e2
        if self.op == '<':
            return e1 < e2



class TupleIndNode():
    def __init__(self, list, ind):
        if(ind.nodeType == 'num'):
            self.list =  list
            self.ind = ind
            self.nodeType = "ind"
        else:
            print("SEMANTIC ERROR")
            #semantic error
    def eval(self):
        return self.list[self.ind.eval()]

class NumNode():
    def __init__(self, val):
        self.val = val
        self.nodeType = "num"
    def eval(self):
        if '.' in self.val:
            return float(self.val)
        else:
            return int(self.val)

def p_line(t):
    """line : expr SEMICOLON"""
    t[0] = t[1].eval()

def p_expr(t):
    """expr : INTEGER
            | FLOAT
            | STRING
            | BOOLEAN
            | list
            | tuple"""
    t[0] = (t[1])

def p_binop_expr(t):
    """expr : expr POW expr
            | expr MULT expr
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

def p_negationOp(t):
    """expr : NOT expr"""
    t[0] = NegationOpNode(t[1],t[2])



def p_sequence(t):
       """sequence : expr COMMA sequence
                   | expr"""
       if len(t) == 4:
           t[0] =  [t[1]] + t[3]
       else:
           t[0] = [t[1]]

def p_list(t):
    """list : L_BRACK sequence R_BRACK
            | L_BRACK R_BRACK"""
    if len(t) == 3:
        t[0] = []
    else:
        t[0] = t[2]

def p_list_index(t):
    """expr : list L_BRACK expr R_BRACK"""
    t[0] = ListIndNode(t[1], t[3])


def p_parenthesized(t):
    """expr : L_PAREN expr R_PAREN"""
    t[0] = t[2]

def p_tuple(t):
    """tuple : L_PAREN sequence R_PAREN
             | L_PAREN R_PAREN"""
    if len(t) == 3:
        t[0] = ()
    else:
        t[0] = tuple(t[2])

def p_tuple_index(t):
    """expr : HASHTAG INTEGER tuple"""
    t[0] = TupleIndNode(t[3], t[2]).eval()

def p_error(t):
    print("SYNTAX ERROR")



# Manually setting precedence and associativity to resolve ambiguity in the
# grammar.
precedence = (


              ('left', 'MINUS','PLUS'),
              ('left', 'L_BRACK'),
              ('left', 'L_PAREN'),
              ('right', 'UMINUS')
              )

parser = yacc.yacc()

while True:
    try:
        s = input("Enter a proposition: ")
    except EOFError:
        break
    if not s:
        continue
    result = parser.parse(s, debug = True)
    print("RESULT:", result)
