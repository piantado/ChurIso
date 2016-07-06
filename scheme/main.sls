#!/usr/bin/env scheme-script
#!r6rs

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;;
;;;; Search for Church Encodings
;;;; Steve Piantadosi -- spiantado@gmail.com -- January 2016
;;;; $ vicare -O2 --source-path . main.sls -- ../domains/boolean.txt 1 1
;;;;
;;;; This outputs a zero on each line so we can sort via
;;;; $ sort -g -z -k4 o.txt > osorted.txt
;;;;
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(import (rnrs)
        (stp-lib)
        (parameters)
        (data)
        (search backtrack)
        (search pq-search))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Some helpful control flow
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; This gives us a for loop with the list of things you loop over at the top.
;; This is for use with for-each, as it doesn't return the list.
(define-syntax for
  (syntax-rules (in)
    ((_ x in y body ...)
     (for-each (lambda (x) body ...)
               y))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; initialize the parameters
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ARGS (command-line))

(if (< (length ARGS) 4)
    (begin
      (displaynerr "Input format must be <base facts> <start> <skip>")
      (exit 1))
    (begin
      (displayn (string-repeat "#" 79)
                "\n# Everyone likes ChurIso!\n"
                (string-repeat "#" 79))))

(define params (initialize-parameters ARGS))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load the data
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define data (read-data-from-file (params 'DATA-FILE) (params 'CONSTRAINT-SORT)))
(displayn (string-repeat "#" 79))
 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; report initial configuration
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(for key in (vector->list (hashtable-keys (params)))
     (displayn "# " key ": " (params key)))
(for key in (vector->list (hashtable-keys (data)))
      (displayn "# " key ": " (data key)))
(displayn (string-repeat "#" 79))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; run the search
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
(displaynerr "# Starting outer stream")
(displayn (string-repeat "#" 79))
(flush-output-port (current-output-port))

(cond
 [(equal? (params 'SEARCH-METHOD) 'backtrack)
  (begin
    (displayn "# backtracking!")
    (for total-complexity-bound in (abrange (data 'COMPLEXITY)
                                            (+ (data 'COMPLEXITY)
                                               (params 'MAX-LENGTH)))
         (call/cc (lambda (exit)
                    (backtrack #t
                               exit
                               (data 'CONSTRAINTS)
                               (data 'DEFINES)
                               total-complexity-bound
                               params
                               data)))))]
 [else
  (begin
    (displayn "# priority queue!")
    (pq-search params data))])
 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; report summary stats and exit
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(displaynerr (string-repeat "#" 79))
(displaynerr "# Backtrack count: " (params 'GLOBAL-BACKTRACK-COUNT))
(displaynerr "# Found count: " (params 'FOUND-COUNT) " for " (cdr ARGS))
(displaynerr (string-repeat "#" 79))
(flush-output-port (current-output-port))
(exit (> (params 'FOUND-COUNT) 0) )
