#!/usr/bin/env scheme-script
#!r6rs

(import (rnrs) (vicare) (stp-lib) (evaluation) (combinators))

;; #####################################################################################
;; #####################################################################################
;; Code to check that evaluation is correct
;; #####################################################################################
;; #####################################################################################

(define (assert-rebracket-equal x y)
  (assert-equal (rebracket x) (rebracket y)))

(assert-rebracket-equal (reduce '(I x)) 'x)
(assert-rebracket-equal (reduce '(K x y)) 'x)
(assert-rebracket-equal (reduce '(S x y z)) '(x z (y z)))
(assert-rebracket-equal (reduce '(C f x y)) '(f y x))
(assert-rebracket-equal (reduce '(B f g x)) '(f (g x)))
(assert-rebracket-equal (reduce '(f (x) y) ) '(f x y))
(assert-rebracket-equal (reduce '((f) (x) y) ) '(f x y))
(assert-rebracket-equal (reduce '(f x y) ) 
              (reduce '((f x) y)))

(assert-rebracket-equal (reduce '(S I I (S I I))) NON-HALT)

(assert-rebracket-equal (reduce '(S (K (S I)) K x y)) '(y x))

(assert-equal #t (trace-equal? '(S I I (S I I)) '(S I I (S I I)) '()))

(assert-equal #t (trace-equal? '(I S I I (S I I)) '(S I I (S I I)) '()))

; For pursuing the Z combinator
;[f] (([x] (f ([v] (x x) v))) ([x] (f ([v] (x x) v))))
;(S (S (S (K S) (S (K K) I)) (K (S (S (K S) (S (K K) (S I I))) (K I)))) (S (S (K S) (S (K K) I)) (K (S (S (K S) (S (K K) (S I I))) (K I)))))

;; Check out a big random composition
;; Here the literal alternative was run using cl
;; http://www.stratigery.com/cl/
(set-MAXes! 10000 10000)
(assert-rebracket-equal (reduce '((K ((K (K (I (((S ((((B ((K ((C ((((K (B (C (I (C (I (((C ((((C ((((((K (I (B (C (((B (C ((C (C ((B (((((K (B (S ((K (I ((C (K (((K (B (C (B ((((I ((C (K (I (C (B ((((B (C (C (I ((C (C (((S ((I ((C (K (((C (C ((S ((K ((B (C ((S (K (B (((S (K (K ((I (K (((I (S (((((I ((S ((C (C ((B (B (S ((S (S ((S (C (K (S (K ((S (K (S (I (K ((K ((B (B (K ((((B (K (((((((((((((K (((S ((S (C (C ((K ((I ((I ((((((((((K ((((((C (((S ((K ((((S (C ((I (B (B ((K (K (C ((I (((B ((B (I (C (K ((K ((B (((K ((((S (((C (S (((B ((B (K ((K (C (((K (((B (((((I ((S ((((S (K (B (K (K ((B ((B (((K (C (((K (B (S (I (K ((S (C (S (K (((C (C (((((S ((((C ((((S (K (K ((((I (B ((C (S ((S (((K (K ((K (C (((((C ((((B (((((B (((S (((((I (B ((((I (((K (K (K (S (S ((((C ((S (C ((C ((K (((((((C ((S (B ((C (I ((S (C (S ((I (B (S ((K ((B ((((B (B (K (I ((((C (C (((((I ((B ((((S ((K ((S ((C (K ((B ((I (C ((K (K ((C (I (S ((((I ((C (C (B (B ((S ((((((K (((S (K (B (S ((((S ((C (((C ((C (((K ((C (B ((B (I (I ((((C (I ((B S) S))) S) S) S)))) B))) B)) I) S)) C)) I) B)) B)) C) K) B))))) C) C)) I) B) I) B) C)) K))))) C)) K) B) S)))) K))) K))) C)) K))) S)) I)) K)) I) C) S)) B)) K) K) C) C))) I) B) K))))) I) B) B)) K)) B)))) C)))) S))) C))) I)) C) C) C) I) B) B)) C)) C))) S)) I) C) I)))))) K) K)) B) K) C))) K) K) K) B)) I) C)) B) B) B) K)) S) I) I)) B) K) C) B))) I))) C) S)) I))) I))) I) C) B)))) C) B) I)) B) S) C)) S) I) K) S))) C) B))))) S)))))) K) K))) K) S)) I)) C)))))) I) B) I)) K)) B) S) I) S)) I) I)) I) C))) K))) K)) C) K))) I) S)) S) B) K)) S) K)) I)) B))))) K)) S) I)) B)))) B)))) C))) C) I) B)) B)) S) B)) B) I) B) K) C)) B) K) S) K) I) B) C) B) I)) I)) B)) K)))) K)) B) S)) S) B) S) C) C) K) S) K) S) B) C) I))) C) K) K)))) S)) I)))))) S)))))) B))) I)))) C))) C)) C)) C) B) S) C))) I) I))) I)))) I) S)))) B))) S)) K)) K))) C) I))) B)) S)) I) B))) B))))) C) K) B)))))) C)) K) S) C))))) B) C))) I))) C)))) B) K) C) I)) B))) S))) B) C))))) S) I) B) S) S)) S) I) C)) S) K))))))) C) S) S)) S)) C)) C) I) B)) K) K)))) S)) S))
                        '(K (C K (C (K (B (S (S (S C))) K)) (S (S (S C)) (K (K (B (S (S (S C))) K))))))))

(displayn "Passed the tests! :)")

;; If we want to print out a new random combinator
;; Note that this requires setting a new seed if we want a new one
;(define (make-random-combinators n)
;  (let ((c (list-ref BASIS (random (length BASIS)))))
;    (if (= n 1)
;        c
;        (if (= (random 2) 0) ; flip on order
;            (list c (make-random-combinators (- n 1)))
;            (list (make-random-combinators (- n 1)) c)))))
;(displayn (make-random-combinators 500))


(exit 0)
