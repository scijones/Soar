from ply import yacc
from gdllex import tokens
from GDL import *
from collapse import collapse
from IR import *
import pdb

ir = IntermediateRep()

def p_rule_list(p):
	'''rule_list : rule rule_list
	             | empty'''
	pass

def p_rule_atomic_sentence(p):
	'''rule : atomic_sentence
	        | LPAREN ARROW atomic_sentence condition_list RPAREN'''

	global ir

	if len(p) == 2:
		ir.add_rule(p[1], [])
	else:
		ir.add_rule(p[3], p[4])

def p_atomic_sentence(p):
	'''atomic_sentence : NAME
		                 | LPAREN NAME term_list RPAREN'''
	if len(p) == 2:
		p[0] = Sentence(p[1], [])
	else:
		p[0] = Sentence(p[2], p[3])

def p_term_list(p):
	'''term_list : empty 
	             | term term_list'''
	if len(p) == 3:
		p[0] = [p[1]] + p[2]
	else:
		p[0] = []

def p_term_variable(p):
	'term : VAR'
	p[0] = Variable(p[1])

def p_term_str_constant(p):
	'term : NAME'
	p[0] = Constant(p[1])

def p_term_num_constant(p):
	'term : NUM'
	p[0] = Constant(p[1])

def p_term_function(p):
	'term : function'
	p[0] = p[1]

def p_function(p):
	'function : LPAREN NAME term_list RPAREN'
	# a function is a predicate
	p[0] = Function(p[2], p[3])

def p_condition_list(p):
	'''condition_list : empty 
	                  | condition condition_list'''
	if len(p) == 3:
		p[0] = [p[1]] + p[2]
	else:
		p[0] = []

def p_condition_literal(p):
	'condition : literal'
	p[0] = p[1]

#def p_condition_distinct(p):
#	'condition : distinct'

def p_condition_or(p):
	'condition : LPAREN OR condition_list RPAREN'
	p[0] = p[3]

def p_literal_atomic_sentence(p):
	'literal : atomic_sentence'
	p[0] = p[1]

def p_literal_not(p):
	'literal : LPAREN NOT atomic_sentence RPAREN'
	p[0] = p[3]
	p[0].negate()

#def p_distinct_var(p):
#	'distinct : LPAREN DISTINCT VAR VAR RPAREN'
#
#def p_distinct_str(p):
#	'distinct : LPAREN DISTINCT VAR NAME RPAREN'
#
#def p_distinct_num(p):
#	'distinct : LPAREN DISTINCT VAR NUM RPAREN'

def p_error(t):
	print "error: %s at line %d" % (t.value, t.lexer.lineno)
	#print "Syntax error on line %d" % p.lineno(1)

def p_empty(p):
	'empty : '
	pass

yacc.yacc()

import sys

if len(sys.argv) < 2:
	print "need kif file"
	sys.exit(1)

file = open(sys.argv[1], 'r').read()

result = yacc.parse(file)

#import psyco
#psyco.full()

try:
	collapse(ir.get_rules())
except KeyboardInterrupt:
	pass
