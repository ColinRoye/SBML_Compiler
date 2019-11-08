
import subprocess
import sys

def assertEq(sbml_line, py_line):
    assert (evalSbml(sbml_line) == str(eval(py_line)))

def assertSyntaxError(sbml_line):
    assert (evalSbml(sbml_line) == 'SYNTAX ERROR')

def assertSemanticError(sbml_line):
    assert (evalSbml(sbml_line) == 'SEMANTIC ERROR')

def evalSbml(sbml_line):
    sbml_output = subprocess.check_output('python3 sbml.py -l \''+sbml_line+'\'', shell=True)
    return str(sbml_output)[2:-3]
# try:
assertEq("1 - 2 + 3;", '1 - 2 + 3')
assertEq("\"hello\" + \"hello\";", "\"hello\" + \"hello\"")
assertEq("[1,[1]][1][0];", "[1,[1]][1][0]")
assertSyntaxError('1 1;')
assertSemanticError('[1][0][0];')
assertSemanticError("[1,[1]][1][0][0];")
assertSemanticError("[1,[]][1][0][0];")


print("SUCCESS")
# except Exception as e:
