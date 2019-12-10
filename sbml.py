#Colin Roye 110378271
import sys
import ply.lex as lex
import ply.yacc as yacc
import readline
# mypl.py
# Tokenizer, Parser, Evaluator for a simple propositional logic

# List of token names

scopeStack = [{}]


tokens = (
          'INTEGER', 'FLOAT',  'BOOLEAN', 'STRING', #DATA TYPES
          'VARIABLE', 'FUN',
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
          'SEMICOLON',
          'LC',
          'RC',
          'IF',
          'WHILE',
          'ELSE',
          'PRINT'
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
t_DIV = r'/'
t_PLUS = r'\+'
t_MINUS = r'-'
t_CONS = r'::'
t_COLON = r':'
t_LTE = r'<='
t_GTE = r'>='
t_EQ_EQ = r'=='
t_NEQ = r'<>'
t_LT = r'<'
t_GT = r'>'
t_EQ   = r'='
t_LC = r'{'
t_RC = r'}'


def t_FUN(t):
    r'fun'
    return t

def t_DIVINT(t):
    r'div'
    return t

def t_MOD(t):
    r'mod'
    return t

def t_IN(t):
    r'in'
    return t

def t_NOT(t):
    r'not'
    return t

def t_AND_ALSO(t):
    r'andalso'
    return t

def t_OR_ELSE(t):
    r'orelse'
    return t

def t_IF(t):
    r'if'
    t.value = t.value
    return t

def t_WHILE(t):
    r'while'
    t.value = t.value
    return t

def t_ELSE(t):
    r'else'
    t.value = t.value
    return t

def t_PRINT(t):
     r'print'
     t.value = t.value
     return t

def t_BOOLEAN(t):
     r'True|False'
     t.value = BoolNode(t.value)
     return t

def t_FLOAT(t):
    r'(((\d*)\.(\d*)((e(-?)\d+)?)))'
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

def t_VARIABLE(t):
     r'[a-zA-Z][a-zA-Z0-9_]*'
     t.value = VarNode(t.value)
     return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

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
        if(isinstance(l, list) or isinstance(l, str)):
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

        if(isinstance((e1), int) and isinstance((e2), int)):
            if self.op == '<=':
                return e1 <= e2
            if self.op == '>=':
                return e1 >= e2
            if self.op == '>':
                return e1 > e2
            if self.op == '<':
                return e1 < e2

        if(isinstance((e1), bool)):
            if self.op == 'not':
                return not e1

        if(isinstance((e1), bool) and isinstance((e2), bool)):
            if self.op == 'andalso':
                return e1 and e2
            if self.op == 'orelse':
                return e1 or e2


class IndexNode():
    def __init__(self, list, ind):
        if(hasattr(list, "name")):
            self.name = list.name;
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


class AssignmentNode():
    def __init__(self, node, val):
        if(isinstance(node, VarNode)):
            self.node = node
            self.name = node.name
            self.val = val
            self.nodeType = "assignemnt"
        else:
            self.node = None
            self.name = None
            self.val = None
            self.nodeType = None
    def eval(self):
        global scope
        if(isinstance(self.node, VarNode)):
            setVal(self.name, reduce(self.val))
            return "assign to var"

class ListAssignmentNode():
    def __init__(self, node, val, inds):
        if(isinstance(node, VarNode)):
            self.node = node
            self.name = node.name
            self.val = val
            self.inds = inds
            self.nodeType = "listAssignment"
        else:
            self.node = None
            self.name = None
            self.val = None
            self.nodeType = None
    def eval(self):
        global scope
        if(isinstance(self.node, VarNode) and getVal(self.name)):
            list = reduce(getVal(self.name))
            temp = list
            for i in range (0, len(self.inds)-1):
                index = reduce(i)
                temp = temp[reduce(self.inds[index])]
            index = reduce(self.inds[-1])
            temp[index] = reduce(self.val)
            setVal(self.name, list)
            return "list assign to var"

class VarNode():
    def __init__(self, name):
        global scope
        self.name = name
        self.nodeType = "variable"
    def eval(self):
        global scope
        if(getVal(self.name) != None):
            return reduce(getVal(self.name))

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
        self.l = val
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

class BlockNode:
    def __init__(self, block):
        # self.expr = expr
        self.block = block
        self.nodeType = "stmt"
    def eval(self):
        temp = ""
        for elm in self.block:
            temp = elm.eval()
            if(temp == None):
                print("SEMANTIC ERROR")
                exit()
        return self

class IfNode:
    def __init__(self, expr, block):
        self.expr = expr
        self.block = block
        self.nodeType = "stmt"
    def eval(self):
        if reduce(self.expr):
            self.block.eval()
        return self

class IfElseNode:
    def __init__(self, expr, ifBlock, elseBlock):
        self.expr = expr
        self.ifBlock = ifBlock
        self.elseBlock = elseBlock
        self.nodeType = "stmt"
    def eval(self):
        if reduce(self.expr):
            self.ifBlock.eval()
        else:
            self.elseBlock.eval()
        return self


class WhileNode:
    def __init__(self, expr, block):
        self.expr = expr
        self.block = block
        self.nodeType = "stmt"
    def eval(self):
        while reduce(self.expr):
            self.block.eval()
        return self


class PrintNode:
    def __init__(self, expr):
        self.expr = expr
        self.nodeType = "stmt"
    def eval(self):
        if(reduce(self.expr) == None):
            print("SEMANTIC ERROR")
            exit()
        print(reduce(self.expr))
        return self

def addScope():
    scopeStack.append({})

def removeScope():
    scopeStack.pop()

def getVal(name):
    for scope in scopeStack[::-1]:
        if name in scope:
            return scope[name]
def setVal(name, val):
    scopeStack[-1][name] = val

class FunNode:
    def __init__(self, name, vars, block, ret):
        self.vars = vars
        self.name = name
        self.block = block
        self.ret = ret
        self.nodeType = "funcallassignment"
    def eval(self, inputVars):
        self.initScope(inputVars)
        self.block.eval()
        output = reduce(self.ret)
        removeScope()
        return output


    def initScope(self, inputVars):
        addScope()
        if(len(self.vars) != len(inputVars)):
            print("SEMANTIC ERROR")
            exit()
        count = 0
        for name in self.vars:
            AssignmentNode(name, inputVars[count]).eval()
            count = count+1




class FunAssignmentNode:
    def __init__(self, name, vars, block, ret):
        self.vars = vars
        self.name = name
        self.block = block
        self.ret = ret
        self.nodeType = "funcallassignment"
    def eval(self):
        #hack###
        setVal(self.name.name, FunNode(self.name, self.vars, self.block, self.ret))
        return "ok"


class FunCallNode:
    def __init__(self, name, sequence):
        self.name = name
        self.sequence = sequence
        self.nodeType = "funcallassignment"
    def eval(self):
        return (getVal(self.name.name).eval(self.sequence))



def p_program(t):
    """program : fun_list block
               | block"""
    # print(t[1][0].block)
    # print(t[1].block)
    # t[1][0].eval()
    # t[1][0].eval()
    if(len(t) == 3):
        for elm in t[1]:
            elm.eval()
        t[2].eval()
    else:
        t[1].eval()
    t[0] = "ok"

def p_fun_list(t):
    """fun_list : fun_list fun_assignment
                | fun_assignment"""
    if(len(t) > 2):
        t[0] = t[1]+ [t[2]]
    else:
        t[0] = [t[1]]


def p_block_stmt_list(t):
    """block_list : block_list stmt_list
                  | stmt_list"""
    if(len(t) > 2):
        t[0] = t[1]+ t[2]
    else:
        t[0] = t[1]

def p_block_list(t):
    """block_list : block_list block
                  | block"""
    if(len(t) > 2):
        t[0] = t[1]+ [t[2]]
    else:
        t[0] = [t[1]]




    if(len(t) > 2):
        t[0] = t[1] + [t[2]]
    else:
        t[0] = [t[1]]


def p_cond_stmt(t):
    """cond_stmt : if
                 | if_else
                 | while"""
    t[0] = t[1]

def p_stmt_list(t):
    """stmt_list : stmt_list stmt
                 | stmt"""
    if(len(t) > 2):
        t[0] = t[1]+ [t[2]]
    else:
        t[0] = [t[1]]

def p_block(t):
    """block : LC block_list RC
             | LC RC"""
    if(len(t) > 3):
        t[0] = BlockNode(t[2])
    else:
        t[0] = BlockNode()

def p_print(t):
    """print : PRINT L_PAREN expr R_PAREN"""
    t[0] = PrintNode(t[3])
def p_stmt(t):
    """stmt : expr SEMICOLON
            | print SEMICOLON
            | assignment SEMICOLON
            | cond_stmt
            | fun_assignment"""
    t[0] = t[1]

def p_if_else(t):
    """if_else : IF L_PAREN expr R_PAREN block ELSE block"""
    t[0] = IfElseNode(t[3], t[5], t[7])

def p_if(t):
    """if : IF L_PAREN expr R_PAREN block"""
    t[0] = IfNode(t[3], t[5])

def p_while(t):
    """while : WHILE L_PAREN expr R_PAREN block"""
    t[0] = WhileNode(t[3], t[5])



def p_expr(t):
    """expr : INTEGER
            | FLOAT
            | STRING
            | BOOLEAN
            | fun_call
            | VARIABLE
            | list
            | tuple"""
    t[0] = (t[1])



def p_fun_call(t):
    """fun_call : VARIABLE L_PAREN sequence R_PAREN
                | VARIABLE L_PAREN  R_PAREN"""
    if(len(t) == 5):
        t[0] = FunCallNode(t[1], t[3]);
    else:
        t[0] = FunCallNode(t[1], []);



def p_fun_assignment(t):
    """fun_assignment : FUN VARIABLE L_PAREN sequence R_PAREN EQ block expr SEMICOLON
                  | FUN VARIABLE L_PAREN R_PAREN EQ block expr SEMICOLON"""
    if(len(t) == 10):
        t[0] = FunAssignmentNode(t[2], t[4], t[7], t[8]);
    else:
        t[0] = FunAssignmentNode(t[2], [], t[6], t[7]);




def p_list_assignment(t):
    """assignment : expr indexSequenceList EQ expr"""
    t[0] = ListAssignmentNode(t[1], t[4], t[2]);

def p_assignment(t):
    """assignment : expr EQ expr"""
    t[0] = AssignmentNode(t[1], t[3]);


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

def p_negationOp(t):
     'expr : NOT expr %prec UMINUS'
     t[0] = BooleanOpNode(t[2] , 'not', '')


def p_index_sequence_list(t):
    """indexSequenceList : indexSequenceList indexTkn
                         | indexTkn"""
    if(len(t) > 2):
        t[0] = t[1]+t[2]
    else:
        t[0] = t[1]

def p_index_tkn(t):
    """indexTkn : L_BRACK expr R_BRACK"""
    t[0] = [t[2]]

def p_list_index(t):
    """expr : expr L_BRACK expr R_BRACK"""
    t[0] = IndexNode(t[1], t[3])

def p_list(t):
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
    exit()

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
           data = fp.read()
           result = parser.parse(data, debug=deb)
           if result != None:
               pass
               # print(result)

except Exception as err:
    print("SEMANTIC ERROR")
    exit()
