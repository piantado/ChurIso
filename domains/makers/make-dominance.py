"""
	Print a dominance hierarchy. Training you get the full NxN grid of relations. 
	Testing you get only the linear one, and can be tested on 

"""
import sys
from itertools import product

def i2s(i):
	return str(unichr(97+i))

for n in xrange(2,5+1):
    with open('../dominance-%s.txt'%n, 'w') as f:
        
        print >>f, """
[unique True %s]
[define True (K K)]
""" % ' '.join(map(i2s, range(n+1)))
        
        for i,j in product(range(n+1), range(n+1)):
            if i == j: continue
            
            if min(i,j)==0 and max(i,j)==n: # catch all d,a pairs
                # a single held-out example
                print >>f, '[show (dom %s %s)]' % ( i2s(i), i2s(j) )
            else:
				if i<j:
					print >>f, '[constrain True = (dom %s %s)]' % ( i2s(i), i2s(j) )
				else:
					print >>f, '[constrain True != (dom %s %s)]' % ( i2s(i), i2s(j) )
     