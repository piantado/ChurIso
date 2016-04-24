#!/usr/bin/env scheme-script
#!r6rs

;; #####################################################################################
;; #####################################################################################
;; Simple backtracking search, that pushes constraints (rhs->lhs) when possible.
;; Steve Piantadosi -- spiantado@gmail.com -- January 2016
;; $ vicare -O2 --source-path . main.sls -- ../domains/boolean.txt 1 1
;;
;; This outputs a zero on each line so we can sort via 
;; $ sort -g -z -k4 o.txt > osorted.txt
;;
;; #####################################################################################
;; #####################################################################################

(import (rnrs) 
        (combinators) (stp-lib) (evaluation) 
        (srfi :41); streams
        )

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Main parameters
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ARGS (command-line))
(if (< (length ARGS) 4)
    (begin
      (displaynerr "Input format must be <base facts> <start> <skip>")
      (exit 1)))

(define DATA-FILE (second ARGS)) ;"data/boolean-quantifier.txt")

;; These two allow us to run searches in parallel. They control the start
;; and the skip for the first defined value. 0,0 will allow it to run normally in a single thread
(define PARALLEL-START (string->number (third ARGS))) 
(define PARALLEL-SKIP  (string->number (fourth ARGS))) 
(displaynerr "# Running with " PARALLEL-START " " PARALLEL-SKIP)

(define MAX-LENGTH 20) ; overall total max for use in search
(define EOR #\nul) ; The end of record. Using #\nul will let you sort -z with multiline output
(define MAX-FIND 10000)
(define PREFIX-DEPTH 25) ; when we say that prefixes must be equal, how deep do they have to be to?
(define COMBINATOR-FILTER 'normal) ; normal (must be normal form), compressed (non-normal forms are okay as long as they are shorter than the normal form), or none (all combinators)

; How do we define uniqueness when we enforce it? 
; Other options include trace-approx-equal? and equal?
(define (are-combinators-equal? lhs rhs)  (normal-form-equal? lhs rhs))

(define COMBINATOR-BASIS '(S K I B C))

(define DISPLAY-INCREMENTAL (or (member "--incremental" ARGS)
                                (member "--verbose" ARGS))) ; display the outermost search to show progress?
(define SHOW-BACKTRACKING   (member "--verbose" ARGS)) ;; show all stages of the backtracking search?

;; Counters
(define GLOBAL-BACKTRACK-COUNT 0) ;; how many times have we called backtrack?
(define found-count 0) ;; how many did we find?

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Load the data
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define all-input (load-file (open-input-file DATA-FILE)))

(define constraints  (map cdr    (filter (lambda (x) (eqv? (first x) 'constrain)) all-input))) ;normal constraints

(define show           (map second (filter (lambda (x) (eqv? (first x) 'show)) all-input)))
(define limits         (map cdr    (filter (lambda (x) (eqv? (first x) 'limit)) all-input)))
(define variables      (apply append  ; just a single list of all variables
                              (map cdr    (filter (lambda (x) (eqv? (first x) 'variable)) all-input))))
(define uniques        (map cdr    (filter (lambda (x) (eqv? (first x) 'unique)) all-input)))
(define defines        (map (lambda (ai) (list (first ai) (reduce (second ai))));; reduce so "showing" works right for things with extra parens
                            (map cdr    (filter (lambda (x) (eqv? (first x) 'define)) all-input))))

; If any of these are seen, their complexity (in printout) is measured by runnign them in this way. 
; So you can search with I, C, B as primitives (for speed) but measure their complexity through S,K (for parsimony)
; Here, we have defined B and C using cl's "tromp" algorithm to minimize the number of S,K
(define defined-combinators '(
                              (I (S K K))
                              (C (S (K (S (K (S S (K K))) K)) S))
                              (B (S (K S) K) )
                              ))


; Sort so that the defines = are always first, since these provide the strongest constraints
(set! constraints (append (filter (lambda (ci) (equal? (second ci) '=)) constraints)
                          (filter (lambda (ci) (not (equal? (second ci) '=))) constraints)))

(displaynerr "# Constraints: " constraints)
(displaynerr "# Uniques " uniques)
(displaynerr "# Variables: " variables)
(displaynerr "# Defines " defines)
(displaynerr "# MAX-LENGTH " MAX-LENGTH)
(displaynerr "# MAX-FIND " MAX-FIND)
(displaynerr "# PREFIX-DEPTH " PREFIX-DEPTH)
(displaynerr "# COMBINATOR-FILTER " COMBINATOR-FILTER)
(displaynerr "# COMBINATOR-BASIS " COMBINATOR-BASIS)
(displaynerr "# DEFINED COMBINATORS " defined-combinators)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Helper functions
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (is-variable? a)
  (member a variables))

(define (uses-variable? a)
  (any (lambda (x) (is-variable? x))
       (flatten a)))

;; like map, but recurses down lists of lists, applying l
(define (sub-f f l)
  (cond [(null? l)  null]
        [(list? l)  (map (lambda (li) (sub-f f li)) l)]
        [ #t        (f l) ]))

(define (substitute t x)
  (sub-f (lambda (a) (cond [(is-variable? a) a] ;; variables are just themselves
                           [(assoc a x) (second (assoc a x))] ;; look up if its in x
                           [ #t a ] ;; otherwise we assume a "quoted" variable. TODO: THIS SHOULD CHANGE
                           ))
         t))

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
; Some helpful control flow, streams
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; This lets us have a for loop with the list of things you loop over
;; up at the top. Note: This is for use with for-each, as it doesn't return the list
(define-syntax for
  (syntax-rules (in)
    ((_ x in y body ...)
     (for-each (lambda (x) body ...)
               y))))      

;; convert s into a stream that has an offset of start
;; and skips skip
;'(import (srfi :41)) ; streams
(define (stream-skip skip s)
  (if (stream-null? s)
      stream-null
      (stream-cons (stream-car s)  
                   (stream-skip skip (stream-drop (+ skip 1) s))))) 

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Manage uniqueness
; We have a list of lists of variables that must be disjoint called uniques
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; look in uniques to see if tok can have value val given the values in x
(define (check-unique-inner tok val uniques x)
  (if (null? uniques)
      #t ;; it's good!
      (let ((l (car uniques)))
        (and (if (member? tok l)
                 (all (lambda (k) (not (are-combinators-equal? (value-of k x 'NA-VALUE) val ))) l) ;; look up each element of l and check that it's value in x is not val
                 #t)
             (check-unique-inner tok val (cdr uniques) x)))))
(define (check-unique tok val x)
  (check-unique-inner tok val uniques x))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Checking equality under reduction
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (normal-form-equal? lhs rhs)
  (let ((reduced-lhs (reduce lhs))
        (reduced-rhs (reduce rhs)))
    (and (is-valid? reduced-lhs) ;; if there is a problem, return; else remove constraint and recurse
         (is-valid? reduced-rhs)
         (equal? (rebracket reduced-rhs) (rebracket reduced-lhs)))))

(define (normal-form-unequal? lhs rhs)
  ; This would be the same as (not (normal-form-equal ..)) except that
  ; we don't want to count NON-HALT as satisfying !=
  (let ((reduced-lhs (reduce lhs))
        (reduced-rhs (reduce rhs)))
    (and (is-valid? reduced-lhs) ;; if there is a problem, return; else remove constraint and recurse
         (is-valid? reduced-rhs)
         (not (equal? (rebracket reduced-rhs) (rebracket reduced-lhs))))))

; check if the partial evaluations are equal
(define (trace-approx-equal? lhs rhs)
  (>= (prefix-check (reduce-partial lhs)
                    (reduce-partial rhs))
      PREFIX-DEPTH))


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

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; How to display a winner
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (display-winner x) 
  
  ;; just gather some info on this -- the lengths of all components
  (define lengths (map (lambda (y) (length (flatten (substitute y defined-combinators))))
                       (map second x)))
  
  ; get a count of running time for the constraints
  (define running-time 
    (let ((start-count (get-reduction-count)))
      (for c in constraints
        (let* ((lhs             (first c))
               (constraint-type (second c))
               (rhs             (third c))
               (lhs-subbed      (substitute (substitute lhs x) defined-combinators))
               (rhs-subbed      (substitute (substitute rhs x) defined-combinators)))
          
          ; and reduce lhs and rhs
          (reduce lhs-subbed)
          (reduce rhs-subbed)))
      
      (- (get-reduction-count) start-count)))
  
  ; Display the total length and running time
  (displayn "number" found-count "\t" (apply max lengths) "\t"  (apply + lengths) "\t" running-time "\t" (* running-time (apply + lengths)) "\t" GLOBAL-BACKTRACK-COUNT)
  
  ; Display the actual values
  (for xi in x
    (displayn ";   " (first xi) " = " (second xi) ))
  (for xi in x
    (displayn (substitute (first xi) defined-combinators) " = " (substitute (second xi) defined-combinators) ))
  
  ; print the show: what you compute, the outcome, and a list of things its equal to
  (for hi in show
    (let ((rho (reduce (substitute hi x))))
      (displayn "showing\t" hi " -> " rho 
                " which equals " (map first (filter (lambda (a) (are-combinators-equal? (second a) rho )) x)) )))
  
  (displayn "---------------------------\n" EOR) ; end in #\0 so we can sort -z by multiple lines
  
  (flush-output-port (current-output-port))
  
  ; kill everything if we've found too many
  (if (> found-count MAX-FIND)
      (begin
        (displaynerr "# Terminating from exceeding MAX-FIND")
        (exit 0))  
      (set! found-count (+ found-count 1)))
  
  
  )

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Main backtracking
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; now constraints can contain lists, where the second element gives the max length* of the combinator we map to
(define (backtrack return constraints x length-bound)
  ; return is a call/cc for backtracking
  ; constraints encodes the current constraints to be defined
  ; x is a list of things that are defined (variable value), an assoc list
  ; first-define is the value of the first constraint. this is handled specially -- 
  ;   it limits the length of other combinators and is split up for multithreading  
  
  (set! GLOBAL-BACKTRACK-COUNT (+ GLOBAL-BACKTRACK-COUNT 1))
  
  (if (null? constraints) 
      (display-winner x) ;; good news!
      (let* ((c           (car constraints))
             (lhs         (first c))
             (constraint-type (second c))
             (rhs         (third c))
             (in-x?       (lambda (a) (assoc a x)))
             (definable   (lambda (a) (and (not (in-x? a)) ; ;what can we define? Must not be defined (not in x) and not be a variable
                                           (not (is-variable? a)))))
             (undefined-lhs (filter definable (flatten lhs)))
             (undefined-rhs (filter definable (flatten rhs)))
             )
        
       ; (displayn "\tc = " c)
       ; (displayn "\tx = " x)
       ; (displayn "\tundefined-rhs = " undefined-rhs)
       ; (displayn "\tundefined-lhs = " undefined-lhs)
        
        (cond           
          ; if we have defined everything
          [(and (null? undefined-lhs) (null? undefined-rhs))
           (if ((cond [(equal? constraint-type '~=) (lambda (lhs rhs) (trace-approx-equal?  (substitute lhs x) (substitute rhs x))) ] 
                      [(equal? constraint-type '=)  (lambda (lhs rhs) (normal-form-equal?   (substitute lhs x) (substitute rhs x))) ]
                      [(equal? constraint-type '^=) (lambda (lhs rhs) (not (normal-form-equal?   (substitute lhs x) (substitute rhs x))))]; okay if they are NON-HALT
                      [(equal? constraint-type '!=) (lambda (lhs rhs) (normal-form-unequal? (substitute lhs x) (substitute rhs x))) ] ; forbid NON-HALT
                      [(equal? constraint-type 'in) (lambda (lhs rhs) (any (lambda (y) (normal-form-equal? y (substitute lhs x)))
                                                                           (substitute rhs x)))] ;lhs reduces to anything on the rhs ;; TODO: This MUST come now after all symbols are defined or else constrain pushing gets confused
                      )
                lhs rhs)     
               (backtrack return (cdr constraints) x length-bound)
               (return))]
          
          ; Push a constraint <-
          [(and (null? undefined-rhs) 
                (not (list? lhs))
                (equal? constraint-type '=))
           (let ((reduced-rhs (rebracket (reduce (substitute rhs x)))))
             ;(displayn "PUSHING " lhs reduced-rhs)
             (if (and (check-unique lhs reduced-rhs x) ;; Enforce uniqueness constraint
                      (is-valid? reduced-rhs)
                      (not (uses-variable? reduced-rhs)) ;; cannot push variable names
                      (<= (length (flatten reduced-rhs)) (value-of lhs limits +inf.0))) ;; enforce depth bound
                 (backtrack return (cdr constraints) (cons (list lhs reduced-rhs) x) length-bound)
                 (return)))] ;;otherwise add and recurse
          
          ; Push a constraint ->
          [(and (null? undefined-lhs) 
                (not (list? rhs))
                (equal? constraint-type '=))
           (let ((reduced-lhs (rebracket (reduce (substitute lhs x)))))
             (if (and (check-unique rhs reduced-lhs x) ;; Enforce uniqueness constraint
                      (is-valid? reduced-lhs)
                      (not (uses-variable? reduced-lhs)) ;;; cannot push variable names
                      (<= (length (flatten reduced-lhs)) (value-of rhs limits +inf.0))) ;; enforce depth bound
                 (backtrack return (cdr constraints) (cons (list rhs reduced-lhs) x) length-bound)
                 (return)))] ;;otherwise add and recurse
          
          
          ;; else we must search
          [ #t  (let* ((to-define (first (append undefined-rhs undefined-lhs))))
                  (stream-for-each (lambda (v) 
                                     ; Display if we should
                                     (if SHOW-BACKTRACKING
                                         (displaynerr (string-repeat "\t" (length x)) ; tab to show progress
                                                      "Trying " to-define "=" v "\t with defines " x ))
                                     ; If we are too long, since we assuem in-order generation, we can return
                                     (if (>= (length (flatten v)) (value-of to-define limits +inf.0))
                                         (return))
                                     
                                     ; else assign and recurse
                                     (call/cc (lambda (ret) (backtrack ret constraints (cons (list to-define v) x) length-bound)))
                                     
                                     null ;; must return a friendly or else scheme goes nuts
                                     )
                                   (stream-filter (lambda (r) (check-unique to-define r x))
                                                  ; This order matters a lot to uniqueness -- must be reduced before we
                                                  ; check uniqueness
                                                  (stream-filter (cond ((eqv? COMBINATOR-FILTER 'normal) is-normal-form?)
                                                                       ((eqv? COMBINATOR-FILTER 'compressed) is-normal-form-or-shorter?)
                                                                       ((eqv? COMBINATOR-FILTER 'none)  (lambda args #t)))
                                                                 (enumerate-all length-bound COMBINATOR-BASIS)))))]
          ))))


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; And run it!
; Here we manage the outermost constraint so we can vary that across
; parallel threads
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(displaynerr "# Starting outer stream") (flush-output-port (current-output-port))
(stream-for-each (lambda (v) 
                   (let* ((c           (car constraints)); figure out what the first thing to define is 
                          (lhs         (first c))
                          (check-equal (equal? (second c) '=))
                          (rhs         (third c))
                          (definable   (lambda (a) (not (is-variable? a))))
                          (undefined-lhs (filter definable (flatten lhs)))
                          (undefined-rhs (filter definable (flatten rhs)))
                          (to-define (first (append undefined-rhs undefined-lhs))))
                     
                     (if DISPLAY-INCREMENTAL
                         (begin
                           (displaynerr "# Trying " to-define " = " v "\t" (length (flatten v)) "\t found\t" found-count)
                           (flush-output-port (current-error-port))))
                     
                     (call/cc (lambda (exit) 
                                (backtrack exit 
                                           constraints 
                                           (cons (list to-define v) defines) 
                                           (length (flatten v)) ; nothing can be longer than the first define in order to ensure efficient search
                                           )))))
                 
                 (stream-filter (cond ((eqv? COMBINATOR-FILTER 'normal) is-normal-form?)
                                      ((eqv? COMBINATOR-FILTER 'compressed) is-normal-form-or-shorter?)
                                      ((eqv? COMBINATOR-FILTER 'none)  (lambda args #t)))
                                (stream-skip PARALLEL-SKIP
                                             (stream-drop PARALLEL-START  ;; handle parallel processing
                                                          (enumerate-all MAX-LENGTH COMBINATOR-BASIS)))))

(displaynerr "# Backtrack count: " GLOBAL-BACKTRACK-COUNT)
(displaynerr "# Found count: " found-count " for " (cdr ARGS))


; Done
(flush-output-port (current-output-port))
(exit (> found-count 0) )
