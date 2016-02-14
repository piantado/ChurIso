#!/usr/bin/env scheme-script
#!r6rs

(import (rnrs) (srfi :41) (vicare) (stp-lib) (evaluation) (combinators))

;; #####################################################################################
;; #####################################################################################
;; Code to check that combinator enumeration is working well
;; #####################################################################################
;; #####################################################################################


; Better get the catalan numbers when '(x) is the basis
(assert-equal (length (stream->list (enumerate-at 1 '(x)))) 
              1)
(assert-equal (length (stream->list (enumerate-at 2 '(x)))) 
              1)
(assert-equal (length (stream->list (enumerate-at 3 '(x)))) 
              2)
(assert-equal (length (stream->list (enumerate-at 4 '(x)))) 
              5)
(assert-equal (length (stream->list (enumerate-at 5 '(x)))) 
              14)
(assert-equal (length (stream->list (enumerate-at 6 '(x)))) 
              42)
(assert-equal (length (stream->list (enumerate-at 7 '(x)))) 
              132)
(assert-equal (length (stream->list (enumerate-at 8 '(x)))) 
              429)
(assert-equal (length (stream->list (enumerate-at 9 '(x)))) 
              1430)
(assert-equal (length (stream->list (enumerate-at 10 '(x)))) 
              4862)

; when there are more combinators
(assert-equal (length (stream->list (enumerate-at 1 '(I S K))))
              (* 1 (expt 3 1)))
(assert-equal (length (stream->list (enumerate-at 2 '(I S K))))
              (* 1 (expt 3 2)))
(assert-equal (length (stream->list (enumerate-at 3 '(I S K))))
              (* 2 (expt 3 3)))
(assert-equal (length (stream->list (enumerate-at 4 '(I S K))))
              (* 5 (expt 3 4)))
(assert-equal (length (stream->list (enumerate-at 5 '(I S K))))
              (* 14 (expt 3 5)))
(assert-equal (length (stream->list (enumerate-at 6 '(I S K))))
              (* 42 (expt 3 6)))

; And we'd better be able to set up a big one without a ton of memory
(displayn "If this hangs, your implementation is not efficient!") (flush-output-port (current-output-port))
(define *do-not-eval-me* (enumerate-at 15 '(x)))
(displayn "Done hang check") (flush-output-port (current-output-port))

;; this can be run to see whether we eat memory for large sizes.
;; We should be able to make it up 
(stream-for-each (lambda (x)
                   (displayn (length (flatten x)) "\t" x))
                 (enumerate-all 15 '(S K I B C) ))

(displayn "Passed the tests! :)")

(flush-output-port (current-output-port))
(exit 0)
