
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftL_BRACKleftL_PARENrightUMINUSAND_ALSO BOOL BOOLEAN COLON COMMA CONS DIV EQ EQ_EQ FLOAT GT GTE HASHTAG IN INTEGER LT LTE L_BRACK L_PAREN MINUS MOD MULT NEQ NOT OR_ELSE PLUS POW R_BRACK R_PAREN SEMICOLON STRINGline : expr SEMICOLONexpr : INTEGER\n            | FLOAT\n            | STRING\n            | list\n            | tupleexpr : expr POW expr\n            | expr MULT expr\n            | expr DIV expr\n            | expr MOD expr\n            | expr PLUS expr\n            | expr MINUS exprexpr : MINUS expr %prec UMINUSexpr : expr AND_ALSO expr\n            | expr OR_ELSE expr\n            | expr LTE expr\n            | expr GTE expr\n            | expr EQ_EQ expr\n            | expr NEQ expr\n            | expr LT expr\n            | expr GT exprexpr : expr IN expr\n            | expr CONS exprexpr : NOT exprsequence : expr COMMA sequence\n                   | exprlist : L_BRACK sequence R_BRACK\n            | L_BRACK R_BRACKexpr : list L_BRACK expr R_BRACKexpr : L_PAREN expr R_PARENtuple : L_PAREN sequence R_PAREN\n             | L_PAREN R_PARENexpr : HASHTAG INTEGER tuple'
    
_lr_action_items = {'INTEGER':([0,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,58,62,],[3,3,3,3,3,39,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,]),'FLOAT':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,58,62,],[4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,]),'STRING':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,58,62,],[5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,]),'MINUS':([0,2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,],[8,19,-2,-3,-4,-5,-6,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,-13,19,-28,19,19,-32,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,-27,8,-30,-31,-33,8,-29,]),'NOT':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,58,62,],[9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,]),'L_PAREN':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,39,58,62,],[11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,62,11,11,]),'HASHTAG':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,58,62,],[12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,]),'L_BRACK':([0,6,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,34,57,58,62,],[10,30,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,-28,-27,10,10,]),'$end':([1,13,],[0,-1,]),'SEMICOLON':([2,3,4,5,6,7,31,32,34,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,57,59,60,61,63,],[13,-2,-3,-4,-5,-6,-13,-24,-28,-32,-7,-8,-9,-10,-11,-12,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-27,-30,-31,-33,-29,]),'POW':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[14,-2,-3,-4,-5,-6,-13,14,-28,14,14,-32,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,-27,-30,-31,-33,-29,]),'MULT':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[15,-2,-3,-4,-5,-6,-13,15,-28,15,15,-32,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,-27,-30,-31,-33,-29,]),'DIV':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[16,-2,-3,-4,-5,-6,-13,16,-28,16,16,-32,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,-27,-30,-31,-33,-29,]),'MOD':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[17,-2,-3,-4,-5,-6,-13,17,-28,17,17,-32,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,-27,-30,-31,-33,-29,]),'PLUS':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[18,-2,-3,-4,-5,-6,-13,18,-28,18,18,-32,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,-27,-30,-31,-33,-29,]),'AND_ALSO':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[20,-2,-3,-4,-5,-6,-13,20,-28,20,20,-32,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,-27,-30,-31,-33,-29,]),'OR_ELSE':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[21,-2,-3,-4,-5,-6,-13,21,-28,21,21,-32,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,-27,-30,-31,-33,-29,]),'LTE':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[22,-2,-3,-4,-5,-6,-13,22,-28,22,22,-32,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,-27,-30,-31,-33,-29,]),'GTE':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[23,-2,-3,-4,-5,-6,-13,23,-28,23,23,-32,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,-27,-30,-31,-33,-29,]),'EQ_EQ':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[24,-2,-3,-4,-5,-6,-13,24,-28,24,24,-32,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,-27,-30,-31,-33,-29,]),'NEQ':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[25,-2,-3,-4,-5,-6,-13,25,-28,25,25,-32,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,-27,-30,-31,-33,-29,]),'LT':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[26,-2,-3,-4,-5,-6,-13,26,-28,26,26,-32,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,-27,-30,-31,-33,-29,]),'GT':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[27,-2,-3,-4,-5,-6,-13,27,-28,27,27,-32,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,-27,-30,-31,-33,-29,]),'IN':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[28,-2,-3,-4,-5,-6,-13,28,-28,28,28,-32,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,-27,-30,-31,-33,-29,]),'CONS':([2,3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,],[29,-2,-3,-4,-5,-6,-13,29,-28,29,29,-32,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,-27,-30,-31,-33,-29,]),'COMMA':([3,4,5,6,7,31,32,34,35,36,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,57,59,60,61,63,],[-2,-3,-4,-5,-6,-13,-24,-28,58,58,-32,-7,-8,-9,-10,-11,-12,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-27,-30,-31,-33,-29,]),'R_BRACK':([3,4,5,6,7,10,31,32,33,34,35,37,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,61,63,64,],[-2,-3,-4,-5,-6,34,-13,-24,57,-28,-26,-32,-7,-8,-9,-10,-11,-12,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,63,-27,-30,-31,-33,-29,-25,]),'R_PAREN':([3,4,5,6,7,11,31,32,34,35,36,37,38,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,57,59,60,61,62,63,64,],[-2,-3,-4,-5,-6,37,-13,-24,-28,-26,59,-32,60,-7,-8,-9,-10,-11,-12,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-27,-30,-31,-33,37,-29,-25,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'line':([0,],[1,]),'expr':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,58,62,],[2,31,32,35,36,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,35,35,]),'list':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,58,62,],[6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,]),'tuple':([0,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,39,58,62,],[7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,61,7,7,]),'sequence':([10,11,58,62,],[33,38,64,38,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> line","S'",1,None,None,None),
  ('line -> expr SEMICOLON','line',2,'p_line','sbml.py',192),
  ('expr -> INTEGER','expr',1,'p_expr','sbml.py',196),
  ('expr -> FLOAT','expr',1,'p_expr','sbml.py',197),
  ('expr -> STRING','expr',1,'p_expr','sbml.py',198),
  ('expr -> list','expr',1,'p_expr','sbml.py',199),
  ('expr -> tuple','expr',1,'p_expr','sbml.py',200),
  ('expr -> expr POW expr','expr',3,'p_binop_expr','sbml.py',204),
  ('expr -> expr MULT expr','expr',3,'p_binop_expr','sbml.py',205),
  ('expr -> expr DIV expr','expr',3,'p_binop_expr','sbml.py',206),
  ('expr -> expr MOD expr','expr',3,'p_binop_expr','sbml.py',207),
  ('expr -> expr PLUS expr','expr',3,'p_binop_expr','sbml.py',208),
  ('expr -> expr MINUS expr','expr',3,'p_binop_expr','sbml.py',209),
  ('expr -> MINUS expr','expr',2,'p_expr_uminus','sbml.py',213),
  ('expr -> expr AND_ALSO expr','expr',3,'p_boolop','sbml.py',217),
  ('expr -> expr OR_ELSE expr','expr',3,'p_boolop','sbml.py',218),
  ('expr -> expr LTE expr','expr',3,'p_boolop','sbml.py',219),
  ('expr -> expr GTE expr','expr',3,'p_boolop','sbml.py',220),
  ('expr -> expr EQ_EQ expr','expr',3,'p_boolop','sbml.py',221),
  ('expr -> expr NEQ expr','expr',3,'p_boolop','sbml.py',222),
  ('expr -> expr LT expr','expr',3,'p_boolop','sbml.py',223),
  ('expr -> expr GT expr','expr',3,'p_boolop','sbml.py',224),
  ('expr -> expr IN expr','expr',3,'p_listop','sbml.py',227),
  ('expr -> expr CONS expr','expr',3,'p_listop','sbml.py',228),
  ('expr -> NOT expr','expr',2,'p_negationOp','sbml.py',232),
  ('sequence -> expr COMMA sequence','sequence',3,'p_sequence','sbml.py',238),
  ('sequence -> expr','sequence',1,'p_sequence','sbml.py',239),
  ('list -> L_BRACK sequence R_BRACK','list',3,'p_list','sbml.py',246),
  ('list -> L_BRACK R_BRACK','list',2,'p_list','sbml.py',247),
  ('expr -> list L_BRACK expr R_BRACK','expr',4,'p_list_index','sbml.py',254),
  ('expr -> L_PAREN expr R_PAREN','expr',3,'p_parenthesized','sbml.py',259),
  ('tuple -> L_PAREN sequence R_PAREN','tuple',3,'p_tuple','sbml.py',263),
  ('tuple -> L_PAREN R_PAREN','tuple',2,'p_tuple','sbml.py',264),
  ('expr -> HASHTAG INTEGER tuple','expr',3,'p_tuple_index','sbml.py',271),
]
