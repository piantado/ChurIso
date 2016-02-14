(library 
 (evaluation)
 (export get-reduction-count reduce rebracket NON-HALT set-MAXes!)
 (import (rnrs) (vicare) (stp-lib) )
 
 ;; #####################################################################################
 ;; #####################################################################################
 ;; Provide evaluation for combinatory logic in normal order
 ;; (arguments are not reduced before being passed)
 ;; #####################################################################################
 ;; #####################################################################################
 
 ;; prevent reductions from getting too long
 (define MAX-LENGTH 100)
 (define MAX-ITER   100) 
 (define (set-MAXes! a b)  ; required by testing module to allow bigger expressions
   (set! MAX-LENGTH a)
   (set! MAX-ITER b))
 
 
 ; what we return for not halting
 (define NON-HALT '*NON-HALT*)
 
 ; count the total number of reductions
 (define REDUCTION-COUNTER 0)
 (define (get-reduction-count) REDUCTION-COUNTER)
 
 ; Reduction of combinators
 ; NOTE: There was a memoized version that is NOT faster
 (define (reduce lst)
   (call/cc (lambda (return)
              (reduce/cc return MAX-ITER lst))))
 
 ;; the kind of reduce you call within call/cc
 (define (reduce/cc return maxn lst)
   (set! REDUCTION-COUNTER (+ REDUCTION-COUNTER 1))
   (cond [(null? lst) '()]
         [(not (list? lst)) lst]
         [(or (<= maxn 0) (> (length lst) MAX-LENGTH)) (return NON-HALT)]
         [ #t (let* ((op (car lst))        ;; what is the first?
                     (args (cdr lst))      ;; what are the args
                     (largs (length args)) ;; how many args
                     (n (- maxn 1)))
                (unlist-singleton 
                 (cond [(and (list? op) #t) (reduce/cc return n (append op args))] ;; ((f x) y) -> (f x y)
                       [(and (eq? op 'I) (>= largs 1)) ; (I x) = x   where x is evaled next
                        (reduce/cc return n args)]
                       [(and (eq? op 'K) (>= largs 2)) ; (K x y) = x
                        (reduce/cc return n (cons (first args) (drop 2 args)))]
                       [(and (eq? op 'S) (>= largs 3)) ; (S x y z) = (x z (y z))
                        (reduce/cc return n (append (list (first args) 
                                                          (third args) 
                                                          (list (second args) (third args))) 
                                                    (drop 3 args)))]
                       [(and (eq? op 'C) (>= largs 3)) ;; (C f x y) = (f y x)
                        (reduce/cc return n (append (list (first args) (third args) (second args))
                                                    (drop 3 args)))]
                       [(and (eq? op 'B) (>= largs 3)) ;; (B f g x) = (f (g x))
                        (reduce/cc return n (append (list (first args) (list (second args) (third args)))
                                                    (drop 3 args)))]
                       
                       [(and (eq? op 'T) (>= largs 2)) ;; (T x y) = (y x)
                        (reduce/cc return n (append (list (second args) (first args))
                                                    (drop 2 args)))]
                       [(and (eq? op 'Y) (>= largs 1)) ;; (Y x) =(x (Y x))
                        (reduce/cc return n (append (list (first args) (list 'Y (first args)))
                                                    (drop 1 args)))]
                       [(and (eq? op 'Z) (>= largs 2)) ;; (Z g v) = (g (Z g) v)
                        (reduce/cc return n (append (list (first args) (list 'Z (first args)) (second args))
                                                    (drop 2 args)))]
                       [(and (eq? op 'W) (>= largs 2)) ;; (W x y) = (x y y)
                        (reduce/cc return n (append (list (first args) (second args) (second args))
                                                    (drop 2 args)))]
                       
                       [ #t (cons op (map (lambda (li) (reduce/cc return n li))
                                          args))]
                       )))]
         ))
 
 ;; evlauation will so conversions like ((S I) S) -> (S I S), following
 ;; the standard combinatory logic conventions. We may want to re-bracket though
 ;; so combinators can be compared to their original form, or displayed as binary
 ;; trees
 (define (rebracket x)
   (if (not (list? x))
       x
       (if (> (length x) 2)
           (rebracket (cons (list (rebracket (first x))
                                  (rebracket (second x)))
                            (cddr x)))
           x)))
 ;(rebracket '(a b c d e f))
 ;(rebracket '((a x v (w z y)) (b y z) c))
 
 )

