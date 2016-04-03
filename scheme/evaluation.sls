(library 
 (evaluation)
 (export get-reduction-count reduce reduce-partial rebracket NON-HALT set-MAXes!)
 (import (rnrs) (vicare) (stp-lib) (rnrs hashtables (6)) )
 
 ;; #####################################################################################
 ;; #####################################################################################
 ;; Provide evaluation for combinatory logic in normal order
 ;; (arguments are not reduced before being passed)
 ;; #####################################################################################
 ;; #####################################################################################
 
 ;; prevent reductions from getting too long
 (define MAX-LENGTH 100)
 (define MAX-ITER   1000) 
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
 
 ; returns a partial list when we 
 (define (reduce-partial lst)
   (reduce/cc '() MAX-ITER lst)) 
 
 
 ;; the kind of reduce you call within call/cc
 (define (reduce/cc return maxn lst)
   ; if return is a null? here, then on recursing too deep we will return NON-HALT 
   ; in the corresponding sub-expression. Otherwise, *only* NON-HALT is returned
  ; (displayn REDUCTION-COUNTER " LST = " lst)
   
   (set! REDUCTION-COUNTER (+ REDUCTION-COUNTER 1))
   
   (cond [(null? lst) '()]
         [(not (list? lst)) lst]
         [(or (<= maxn 0) (> (length* lst) MAX-LENGTH))
          (if (null? return) lst (return NON-HALT))]
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
                       
;                       [(and (eq? op 'T) (>= largs 2)) ;; (T x y) = (y x)
;                        (reduce/cc return n (append (list (second args) (first args))
;                                                    (drop 2 args)))]
;                       [(and (eq? op 'Y) (>= largs 1)) ;; (Y x) =(x (Y x))
;                        (reduce/cc return n (append (list (first args) (list 'Y (first args)))
;                                                    (drop 1 args)))]
;                       [(and (eq? op 'Z) (>= largs 2)) ;; (Z g v) = (g (Z g) v)
;                        (reduce/cc return n (append (list (first args) (list 'Z (first args)) (second args))
;                                                    (drop 2 args)))]
;                       [(and (eq? op 'W) (>= largs 2)) ;; (W x y) = (x y y)
;                        (reduce/cc return n (append (list (first args) (second args) (second args))
;                                                    (drop 2 args)))]
                       ;                       
                       [ #t (cons op (map (lambda (li) (reduce/cc return (floor (/ n (length args))) li)) ;; divide up evaluation steps n among each, or else we can get huge explosions in running time
                                          args))]
                       )))]
         ))
 
 
 ;; the kind of reduce you call within call/cc
 ; (define (reduce/cc return maxn trace-hash lst)  
 ;   
 ;   ; if trace-stack is provided, we store all stages of the evaluation in it
 ;   ; so that we can 
 ;   (if (hashtable? trace-hash)
 ;       (hashtable-set! trace-hash (rebracket lst) 1))
 ;   
 ;   (set! REDUCTION-COUNTER (+ REDUCTION-COUNTER 1))
 ;   (cond [(null? lst) '()]
 ;         [(not (list? lst)) lst]
 ;         [(or (<= maxn 0) (> (length lst) MAX-LENGTH)) (return NON-HALT)]
 ;         [ #t (let* ((op (car lst))        ;; what is the first?
 ;                     (args (cdr lst))      ;; what are the args
 ;                     (largs (length args)) ;; how many args
 ;                     (n (- maxn 1)))
 ;                (rebracket
 ;                (unlist-singleton (if (not (member op '(I K S C B T Y Z W)))
 ;                                      (cons op (map (lambda (li) (reduce/cc return n #f li)) ; no trace hash in sub-expressions
 ;                                                    args))
 ;                                      
 ;                                      (reduce/cc return 
 ;                                                 n 
 ;                                                 trace-hash 
 ;                                                 (cond [(and (list? op) #t) (append op args)] ;; ((f x) y) -> (f x y)
 ;                                                       [(and (eq? op 'I) (>= largs 1)) args] ; (I x) -> x
 ;                                                       [(and (eq? op 'K) (>= largs 2)) (cons (first args) (drop 2 args))]; (K x y) = x
 ;                                                       [(and (eq? op 'S) (>= largs 3)) (append (list (first args) 
 ;                                                                                                     (third args) 
 ;                                                                                                     (list (second args) (third args))) 
 ;                                                                                               (drop 3 args))] ; (S x y z) = (x z (y z))
 ;                                                       [(and (eq? op 'C) (>= largs 3)) (append (list (first args) (third args) (second args))
 ;                                                                                               (drop 3 args))] ;; (C f x y) = (f y x)
 ;                                                       [(and (eq? op 'B) (>= largs 3))  (append (list (first args) (list (second args) (third args)))
 ;                                                                                                (drop 3 args))] ;; (B f g x) = (f (g x))
 ;                                                       [(and (eq? op 'T) (>= largs 2)) (append (list (second args) (first args))
 ;                                                                                               (drop 2 args))] ;; (T x y) = (y x)
 ;                                                       [(and (eq? op 'Y) (>= largs 1))  (append (list (first args) (list 'Y (first args)))
 ;                                                                                                (drop 1 args))] ;; (Y x) =(x (Y x))
 ;                                                       [(and (eq? op 'Z) (>= largs 2)) (append (list (first args) (list 'Z (first args)) (second args))
 ;                                                                                               (drop 2 args))] ;; (Z g v) = (g (Z g) v)
 ;                                                       [(and (eq? op 'W) (>= largs 2)) (append (list (first args) (second args) (second args))
 ;                                                                                               (drop 2 args))] ;; (W x y) = (x y y)                                  
 ;                                                       ))
 ;                                      ))))]))
 
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

