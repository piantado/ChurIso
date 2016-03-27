"""
Convert dot format to python
"""
import re 
from collections import defaultdict 
import itertools

PATH = "body.dot"

with open(PATH, 'rt') as f:
	lines = f.readlines()


adjacent = defaultdict(lambda : False)
tokens = set()
for l in lines:
	if not re.search("[a-z]", l): continue 

	lhs, rhs = re.split(r"\s*->\s*", l.strip())
	
	for ll in re.split(r"\s+", lhs):
		tokens.add(ll)
		for rr in re.split(r"\s+", rhs):
			tokens.add(rr)
			adjacent[ (ll, rr) ] = True
			
# Now print
print "[unique True False adjacent %s]" % ' '.join(tokens)

for tf in [True, False]: # print trues before falses
	for x,y in itertools.product(tokens, tokens):
		if adjacent[(x,y)] == tf:
			# enforce true/false
			#print "[constrain %s = (adjacent %s %s)]" % (adjacent[ (x,y) ], x, y)
			
			# enforce equality and non
			print "[constrain True %s (adjacent %s %s)]" % ('=' if adjacent[ (x,y) ] else '!=', x, y)
		
	