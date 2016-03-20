(library 
 (evaluation)
 (export get-reduction-count reduce rebracket NON-HALT set-MAXes! reduce-with-hash reduce-one)
 (import (rnrs) (vicare) (stp-lib) (rnrs hashtables (6)) )
 
 ;; #####################################################################################
 ;; #####################################################################################
 ;; Provide evaluation for combinatory logic in normal order
 ;; (arguments are not reduced before being passed)
 ;; #####################################################################################
 ;; #####################################################################################
 
 ;; prevent reductions from getting too long
 (define MAX-LENGTH 100)
 (define MAX-ITER   25) 
 (define (set-MAXes! a b)  ; required by testing module to allow bigger expressions
   (set! MAX-LENGTH a)
   (set! MAX-ITER b))
 
 
 ; what we return for not halting
 (define NON-HALT '*NON-HALT*)
 
 ; count the total number of reductions
 (define REDUCTION-COUNTER 0)
 (define (get-reduction-count) REDUCTION-COUNTER)
 
 
 
 ; Just check my evaluation path
 (define (reduce-with-hash lst)
   (let ((trace-hash (make-hashtable (lambda args 1) equal?)))
     
     (define (recurse lst n)
       (if (> n MAX-ITER)
           NON-HALT
           (let ((r (reduce-one lst)))
             (hashtable-set! trace-hash (rebracket r) #t)
             (if (equal? r lst)
                 r
                 (recurse r (+ n 1))))))
     
     (recurse lst 0) ; don't care about this here
     
     trace-hash))
    
 
 
 
 ;; Standard reduction
 (define (reduce lst)
   
   (define (recurse lst n)
     (if (> n MAX-ITER)
         NON-HALT
         (let ((r (reduce-one lst)))
           (if (equal? r lst)
               r
               (recurse r (+ n 1))))))
   
   (recurse lst 0))

 

 (define (reduce-one lst)
    ; do a single outermost leftmost reduction
   ; This is slower than the older eval code in github, but allows us to easily 
   ; examine the state after each step of reduction
   
   ;(displayn "Reducing one " lst)
   (cond [(not (list? lst)) lst]
         [(null? lst) '()]
         [(= (length lst) 1) (reduce-one (car lst))] 
         [ #t (let* ((op   (car lst))
                     (args (cdr lst))
                     (largs (length args)))
                (if (list? op)
                    (append op (if (list? args) args (list args))) ; gobbling rule
                    (cond [(and (eq? op 'I) (>= largs 1))
                           args]
                          [(and (eq? op 'K) (>= largs 2)) 
                           (cons (first args)  (drop 2 args))]
                          [(and (eq? op 'S) (>= largs 3))
                           (append (list (first args) 
                                         (third args) 
                                         (list (second args) (third args))) 
                                   (drop 3 args))]
                          ;[#t (cons op (reduce-one args))])))]))
                          [#t (cons op (map reduce-one args))] ; apply one step in parallel
                          
                          )))])) ; 
 
 
 
 

 (define (reduce/cc return maxn partial-evaluation-callback lst)
   
   ;; call this each partial eval
   (partial-evaluation-callback lst)
 
   ; always update our coutner
   (set! REDUCTION-COUNTER (+ REDUCTION-COUNTER 1))
   
   ; and evaluate l
   (cond [(null? lst) '()]
         [(not (list? lst)) lst]
         [(or (<= maxn 0) (> (length lst) MAX-LENGTH)) (return NON-HALT)]
         [ #t (let* ((op (car lst))        ;; what is the first?
                     (args (cdr lst))      ;; what are the args
                     (largs (length args)) ;; how many args
                     (n (- maxn 1)))
                (unlist-singleton 
                 (cond [(and (list? op) #t) 
                        (reduce/cc return n partial-evaluation-callback (append op args))] ;; ((f x) y) -> (f x y)
                       [(and (eq? op 'I) (>= largs 1)) ; (I x) = x   where x is evaled next
                        (reduce/cc return n partial-evaluation-callback args)]
                       [(and (eq? op 'K) (>= largs 2)) ; (K x y) = x
                        (reduce/cc return n partial-evaluation-callback (cons (first args)  (drop 2 args)))]
                       [(and (eq? op 'S) (>= largs 3)) ; (S x y z) = (x z (y z))
                        (reduce/cc return n partial-evaluation-callback (append (list (first args) 
                                                          (third args) 
                                                          (list (second args) (third args))) 
                                                    (drop 3 args)))]
                       [(and (eq? op 'C) (>= largs 3)) ;; (C f x y) = (f y x)
                        (reduce/cc return n partial-evaluation-callback (append (list (first args) (third args) (second args))
                                                    (drop 3 args)))]
                       [(and (eq? op 'B) (>= largs 3)) ;; (B f g x) = (f (g x))
                        (reduce/cc return n partial-evaluation-callback (append (list (first args) (list (second args) (third args)))
                                                    (drop 3 args)))]
                       
                       [(and (eq? op 'T) (>= largs 2)) ;; (T x y) = (y x)
                        (reduce/cc return n partial-evaluation-callback (append (list (second args) (first args))
                                                    (drop 2 args)))]
                       [(and (eq? op 'Y) (>= largs 1)) ;; (Y x) =(x (Y x))
                        (reduce/cc return n partial-evaluation-callback (append (list (first args) (list 'Y (first args)))
                                                    (drop 1 args)))]
                       [(and (eq? op 'Z) (>= largs 2)) ;; (Z g v) = (g (Z g) v)
                        (reduce/cc return n partial-evaluation-callback (append (list (first args) (list 'Z (first args)) (second args))
                                                    (drop 2 args)))]
                       [(and (eq? op 'W) (>= largs 2)) ;; (W x y) = (x y y)
                        (reduce/cc return n partial-evaluation-callback (append (list (first args) (second args) (second args))
                                                    (drop 2 args)))]
                       
                        [ #t (let* ((therest (map (lambda (li) (reduce/cc return n (lambda a #t) li)) args))
                                    (ret (cons op therest)))
                               (partial-evaluation-callback ret) ; won't be called elsewhere
                               ret)]
                                    
                                    
                       ; wrap in my op into the partial callback
                      ; [ #t (cons op (map (lambda (li) (reduce/cc return n (lambda (l) (partial-evaluation-callback (cons op l))) li))
                      ;                    args))]
                       )))]
         ))
 
 
 ;; the kind of reduce you call within call/cc
 (define (reduce/cc-XXXX return maxn trace-hash lst)  
   
   ; if trace-stack is provided, we store all stages of the evaluation in it
   ; so that we can 
   (if (hashtable? trace-hash)
       (hashtable-set! trace-hash (rebracket lst) 1))
   
   (set! REDUCTION-COUNTER (+ REDUCTION-COUNTER 1))
   (cond [(null? lst) '()]
         [(not (list? lst)) lst]
         [(or (<= maxn 0) (> (length lst) MAX-LENGTH)) (return NON-HALT)]
         [ #t (let* ((op (car lst))        ;; what is the first?
                     (args (cdr lst))      ;; what are the args
                     (largs (length args)) ;; how many args
                     (n (- maxn 1)))
                (rebracket
                (unlist-singleton (if (not (member op '(I K S C B T Y Z W)))
                                      (cons op (map (lambda (li) (reduce/cc return n #f li)) ; no trace hash in sub-expressions
                                                    args))
                                      
                                      (reduce/cc return 
                                                 n 
                                                 trace-hash 
                                                 (cond [(and (list? op) #t) (append op args)] ;; ((f x) y) -> (f x y)
                                                       [(and (eq? op 'I) (>= largs 1)) args] ; (I x) -> x
                                                       [(and (eq? op 'K) (>= largs 2)) (cons (first args) (drop 2 args))]; (K x y) = x
                                                       [(and (eq? op 'S) (>= largs 3)) (append (list (first args) 
                                                                                                     (third args) 
                                                                                                     (list (second args) (third args))) 
                                                                                               (drop 3 args))] ; (S x y z) = (x z (y z))
                                                       [(and (eq? op 'C) (>= largs 3)) (append (list (first args) (third args) (second args))
                                                                                               (drop 3 args))] ;; (C f x y) = (f y x)
                                                       [(and (eq? op 'B) (>= largs 3))  (append (list (first args) (list (second args) (third args)))
                                                                                                (drop 3 args))] ;; (B f g x) = (f (g x))
                                                       [(and (eq? op 'T) (>= largs 2)) (append (list (second args) (first args))
                                                                                               (drop 2 args))] ;; (T x y) = (y x)
                                                       [(and (eq? op 'Y) (>= largs 1))  (append (list (first args) (list 'Y (first args)))
                                                                                                (drop 1 args))] ;; (Y x) =(x (Y x))
                                                       [(and (eq? op 'Z) (>= largs 2)) (append (list (first args) (list 'Z (first args)) (second args))
                                                                                               (drop 2 args))] ;; (Z g v) = (g (Z g) v)
                                                       [(and (eq? op 'W) (>= largs 2)) (append (list (first args) (second args) (second args))
                                                                                               (drop 2 args))] ;; (W x y) = (x y y)                                  
                                                       ))
                                      ))))]))
 
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

