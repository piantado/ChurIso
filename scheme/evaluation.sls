(library 
 (evaluation)
 (export get-reduction-count reduce reduce-partial rebracket NON-HALT set-MAXes!
         is-normal-form? is-normal-form-or-shorter?
         are-combinators-equal? normal-form-equal? not-normal-form-equal?
         normal-form-unequal? trace-approx-equal? normal-form-in? is-valid?)
 (import (rnrs) (vicare) (stp-lib))
 
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

; we only have to look at combinators that cannot be reduced, since
; if they can be reduced, we will find them elsewhere in the search
; This means that when try something, we never will need to reduce it
(define (is-normal-form? x)
  (let ((rx (reduce x)))
    (and (is-valid? rx)
         (equal? (rebracket rx) x))))

(define (is-normal-form-or-shorter? x)
  ; True if its a normal form OR its normal form is longer then its current form
  ; This is useful for excluding 
  (let ((rx (reduce x)))
    (and (is-valid? rx)
         (<= (length* rx) (length* x)))))

; check if a reduced form is "valid"     
(define (is-valid? x)
  (and (not (equal? x NON-HALT))
       (not (equal? x '()))))


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Checking equality under reduction
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (are-combinators-equal? lhs rhs parameter)
  (cond
   [(equal? 'normal (parameter 'COMBINATOR-EQUALITY))
    (normal-form-equal? lhs rhs)]
   [(equal? 'trace (parameter 'COMBINATOR-EQUALITY))
    (trace-approx-equal? lhs rhs parameter)]
   [(equal? 'equal (parameter 'COMBINATOR-EQUALITY))
    (equal? lhs rhs)]))

(define (normal-form-equal? lhs rhs)
  (let ((reduced-lhs (reduce lhs))
        (reduced-rhs (reduce rhs)))
    (and (is-valid? reduced-lhs) ;; if there is a problem, return; else remove constraint and recurse
         (is-valid? reduced-rhs)
         (equal? (rebracket reduced-rhs) (rebracket reduced-lhs)))))

(define (not-normal-form-equal? lhs rhs)
  (not (normal-form-equal? lhs rhs)))

(define (normal-form-unequal? lhs rhs)
  ; This would be the same as (not (normal-form-equal ..)) except that
  ; we don't want to count NON-HALT as satisfying !=
  (let ((reduced-lhs (reduce lhs))
        (reduced-rhs (reduce rhs)))
    (and (is-valid? reduced-lhs) ;; if there is a problem, return; else remove constraint and recurse
         (is-valid? reduced-rhs)
         (not (equal? (rebracket reduced-rhs) (rebracket reduced-lhs))))))

; check if the partial evaluations are equal
(define (trace-approx-equal? lhs rhs params)
  (>= (prefix-check (reduce-partial lhs)
                    (reduce-partial rhs))
      (params 'PREFIX-DEPTH)))

(define (normal-form-in? x ys)
  (any (lambda (y) (normal-form-equal? x y))
       ys))

(define (prefix-check x y)
  ; how many symbols do I have to look at to find they're not equal?
  ; (NOTE: I check the lengths of each list, so I may not need to recurse into it. 
  (if (or (not (eqv? (list? x) (list? y)))
          (and (list? x) (not (= (length x) (length y)))))
      0
      (if (list? x)
          (if (null? x) ; and must have null y
              +inf.0
              (+ 1 (min (prefix-check (car x) (car y))
                        (prefix-check (cdr x) (cdr y)))))
          (if (eqv? x y) +inf.0 0))))
;(prefix-check '(a a (c d) b (a b (c d))) '(a a (d d) b (a b (c d e))))

;(displayn (trace-approx-equal? '((S (K (S I I)) (S (S (K S) K) (K (S I I)))) f)
;                               '(f ((S (K (S I I)) (S (S (K S) K) (K (S I I)))) f))
;                               ))

 
 )
