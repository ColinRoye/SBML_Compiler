

import sys, subprocess

def check(sbml_line, py_line):
    assert(execfile('sbml.py', 'input')== exec(py_line));


check("1 - 2 + 3;", "1 - 2 + 3")
