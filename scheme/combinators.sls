(library 
 (combinators)
 (export enumerate-all enumerate-at enumerate-at-split Isk Bsk Csk)
 (import (rnrs) (srfi :41) (vicare) )
 
 ;; #####################################################################################
 ;; #####################################################################################
 ;; Code for enumerating all combinators
 ;; #####################################################################################
 ;; #####################################################################################
 
 
 ; define some kinds of combinators that can be used
 ; in our basis, if we want
 (define Isk '(S K K))
 (define Csk '(S (S (K S) (S (K K) (S (K S) (S (S (K S) (S (K K) (S K K))) (K (S K K)))))) (K (S (K K) (S K K)))))
 (define Bsk '(S (S (K S) (S (K K) (S (K S) (S (K K) (S K K))))) (K (S (S (K S) (S (K K) (S K K))) (K (S K K))))))
 
 ;; We might be able to improve this by thinking about each integer as a code
 ;; for a tree, and then just iterating integers and translating them to trees
 ;; otherwise, this stream crossing stuff is expensive and hard. 
 
 ; Stream of all up to n
 (define (enumerate-all nmax BASIS)
   (define (enumerate-all* n nmax BASIS)
     (if (>= n nmax)  
         stream-null
         (stream-append (enumerate-at n BASIS)
                        (enumerate-all* (+ n 1) nmax BASIS))))
   (enumerate-all* 1 nmax BASIS))

;; Make a recursive loop over all k
 (define (enumerate-at-split n k BASIS)
   ; Enumerate at a given depth n, splitting k off to one side
   (cond [(= n 1) (list->stream BASIS) ]
         [ #t     (stream-of (list x y)
                             (x in (enumerate-at k BASIS))
                             (y in (enumerate-at (- n k) BASIS)))]))
 
 
 (define (enumerate-at n BASIS)
   ;; over every possible split
   (define (enum-rec n k)
     (stream-append (enumerate-at-split n k BASIS)
                    (if (> k 1)
                        (enum-rec n (- k 1))
                        stream-null)))   
   
   (enum-rec n (- n 1)))
 
 
 )