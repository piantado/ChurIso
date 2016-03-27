"""
    Generate data for quantifiers, varying the number of examples from 1 to 5
    This print training of length n, then testing of length n+1, n+2, n+3, etc.
    
    NOTE: We get O(N^2) if we do both forall and every; O(N) if separate
"""


from itertools import product

def print_data_for(stop, istraining, f):
    
    for n in xrange( 2**stop ):
        b = bin(n)[2:] # skip the first 2 of bin, which is '0b'
        digits = [False]*(stop-len(b))
        digits.extend([ d=='1' for d in b]) 
        
        ## we hold out the higher ones
        if istraining:
            #print >>f, '[constrain %s (forall %s)]' % ( all(digits), ' '.join(map(str, digits))  )
            
            if any(digits):
                
                print >>f, '[constrain True = (exists %s)]' % ' '.join(map(str, digits))
                
            else:
                print >>f, '[constrain True != (exists %s)]' %  ' '.join(map(str, digits))
            
        else:
            #print >>f, '[show (forall %s)]' % ( ' '.join(map(str, digits))  )
            print >>f, '[show (exists %s)]' % ( ' '.join(map(str, digits))  )
        
        #print digits
    
for t in xrange(1,6+1):
    with open('../quantifiers-%s.txt'%t, 'w') as f:
        print >>f, "[define True (K K)]"
        print >>f, "[define False (K)]"
        print >>f, "[unique exists forall]"
        
        
        print_data_for(t, True, f)
        print_data_for(t+1, False, f)
        print_data_for(t+2, False, f)
        print_data_for(t+3, False, f)
