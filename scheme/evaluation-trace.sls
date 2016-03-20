; A version of evaluation that includes a trace of each step. 

(library 
 (evaluation)
 (export reduce-trace reduce reduce-one)
 (import (rnrs) (vicare) (stp-lib) (rnrs hashtables (6)) )
 
 
 
 ; return the entire evaluation path
 (define (reduce-trace lst)
   (define (recurse lst n)
     (if (> n MAX-ITER)
         null
         (let ((r (reduce-one lst)))
           (if (equal? r lst)
               r
               (cons r (recurse r (+ n 1)))))))
   (recurse lst 0) ; don't care about this here
   )
 
 
 
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
 )