; Old version of optimizing constraint order by 
; random shuffling 
(library 
 (data randomize)
 (export random-optimize-constraints)
 (import (rnrs) (vicare) (stp-lib) )

 (define N-CONSTRAINT-OPTIMIZE 10000) ; how many random orders do we try to find the best cosntraint order?

 (define (random-optimize-constraints compute-complexity constraints defines)
   
   (let ((best-order constraints)
         (best-k  (compute-complexity constraints defines)))
   
     (map (lambda (xx)
            
            (set! constraints (shuffle constraints))
            
            (let ((k (compute-complexity constraints defines)))
              (if (< k best-k)
                  (begin
                    (set! best-order constraints)
                    (set! best-k k))))
            )
           (range N-CONSTRAINT-OPTIMIZE))
     
     best-order))
 
 
 )
