"""
	Print a data for a CFG 

"""
import sys
import re
from itertools import product


SHOW = 4 # how many deeper to show?

for N in xrange(1,9):

    lst = ['']

    with open('../anbn-%s.txt'%N, 'w') as f:
        #print >>f, "[define True (K K)]"

        for n in xrange(1, N+SHOW+1):

            nxtlst = ['a'+x for x in lst] + ['b'+x for x in lst]
            lst = nxtlst

            for x in lst:
                the_as = re.match(r'^(a+)(b+)', x)
                if the_as and \
                                len(the_as.groups(None)[0]) == len(the_as.groups(None)[1]) and \
                                len(the_as.groups(None)[1]) == float(len(x))/2:
                    op = "="
                else:
                    op = "!="

                if len(x) <= N:
                    print >>f, '[constrain True %s (check %s)]' % ( op,  ' '.join([xi for xi in x])) # add spaces
                else:
                    print >>f, '[show (check %s)]' % ' '.join([xi for xi in x])

