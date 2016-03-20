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
;; Note: The evaluator can't figure out (car %x %y) -> %x, only accepting -> (%x). 
;; #####################################################################################
;; #####################################################################################

;; TO DO LIST:
;; Domain to add: c-command!!
;; or a recursive function that doesn't use variables for the Y combinator
; fix the parens and a^nb^n forma languages etc

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
      (displaynerr "Input format must be <constraint file> <start> <skip>")
      (exit 1)))

(define DATA-FILE (second ARGS)) ;"data/boolean-quantifier.txt")

;; These two allow us to run searches in parallel. They control the start
;; and the skip for the first defined value. 0,1 will allow it to run normally
;; in a single thread. 
(define PARALLEL-START (string->number (third ARGS))) ;; start from 0?
(define PARALLEL-SKIP  (string->number (fourth ARGS))) ;; skip 1
(displayn "# Running with " PARALLEL-START " " PARALLEL-SKIP)

(define MAX-LENGTH 20) ; overall total max
(define EOR #\nul) ; The end of record. Using #\nul will let you sort -z 
(define MAX-FIND 1000)
(define REQUIRE-REDUCED #t) ; must the combinators we associate with symbols be reduced?
(define COMBINATOR-BASIS '(I S K)); B C))

(define DISPLAY-INCREMENTAL (or (member "--incremental" ARGS)
                                (member "--verbose" ARGS))) ; display the outermost search to show progress?
(define SHOW-BACKTRACKING   (member "--verbose" ARGS)) ;; show all stages of the backtracking search?

;; Coutners
(define GLOBAL-BACKTRACK-COUNT 0) ;; how many times have we called backtrack?
(define found-count 0) ;; how mamny did we find?

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

(displaynerr "# Constraints: " constraints)
(displaynerr "# Uniques " uniques)
(displaynerr "# Variables: " variables)
(displaynerr "# Defines " defines)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Helper functions
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (is-variable? a)
  (member a variables))

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
(define (irreducible x)
  (let ((rx (reduce x)))
    (and (is-valid? rx)
         (equal? (rebracket rx) x))))

; check if a reduced form is "valid"     
(define (is-valid? x)
  (and (not (equal? x NON-HALT))
       (not (equal? x '()))))

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
                   (stream-skip skip (stream-drop skip s))))) 

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
                 (all (lambda (k) (not (equal? (value-of k x 'NA-VALUE) val))) l) ;; look up each element of l and check that it's value in x is not val
                 #t)
             (check-unique-inner tok val (cdr uniques) x)))))
(define (check-unique tok val x)
  (check-unique-inner tok val uniques x))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Checking equality under reduction
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (normal-form-equal? lhs rhs x)
  (let ((reduced-lhs (reduce (substitute lhs x)))
        (reduced-rhs (reduce (substitute rhs x))))
    (and (is-valid? reduced-lhs) ;; if there is a problem, return; else remove constraint and recurse
         (is-valid? reduced-rhs)
         (equal? (rebracket reduced-rhs) (rebracket reduced-lhs)))))

(define (normal-form-unequal? lhs rhs x)
  (not (normal-form-equal? lhs rhs x)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; How to display a winner
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (display-winner x) 
  
  ;; just gather some info on this -- the lengths of all components
  (define lengths (map (lambda (y) (length (flatten y)))
                       (map second x)))
  
  ; get a count of running time for the constraints
  (define running-time 
    (let ((start-count (get-reduction-count)))
      (for ci in constraints
        (reduce (substitute ci x)))
      (- (get-reduction-count) start-count)))
  
  ; Display the total length and running time
  (displayn found-count "\t" (apply + lengths) "\t" (apply max lengths) "\t" running-time "\t" GLOBAL-BACKTRACK-COUNT)
  
  ; Display the actual values
  (for xi in x
    (displayn "let\t" (first xi) "\t" (second xi) ))
  
  ; print the show: what you compute, the outcome, and a list of things its equal to
  (for hi in show
    (let ((rho (reduce (substitute hi x))))
      (displayn "showing\t" hi " -> " rho " which equals " (map first (filter (lambda (a) (equal? (second a) rho)) x)) )))
  
  ; and another format
  (displayn "# " x )
  (displayn "---------------------------\n" EOR) ; end in #\0 so we can sort -z by multiple lines
  
  ; kill everything if we've found too many
  (if (> found-count MAX-FIND)
      (begin
        (displayn "# Terminating from exceeding MAX-FIND")
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
        
        ;(displayn "c = " c)
        ;(displayn "x = " x)
        ;(displayn "undefined-rhs = " undefined-rhs)
        ;(displayn "undefined-lhs = " undefined-lhs)
        
        (cond 
          
          ; if we have defined everything
          [(and (null? undefined-lhs) (null? undefined-rhs))
           (if ((cond ;[(equal? constraint-type '=b=) trace-equal?] ; Not implemented
                      [(equal? constraint-type '=)   normal-form-equal?]
                      [(equal? constraint-type '!=)  normal-form-unequal?])
                lhs rhs x)     
               (backtrack return (cdr constraints) x length-bound)
               (return))]

          ;; we can push a constraint TODO: MAKE THIS WORK IN EITHER ORDER
          ;; NOTE: WE do *not* enforce the length bound here
          [(and (null? undefined-rhs) (not (list? lhs))) 
           (let ((reduced-rhs (rebracket (reduce (substitute rhs x)))))
             (if (and (check-unique lhs reduced-rhs x) ;; Enforce uniqueness constraint
                      (is-valid? reduced-rhs)
                      (<= (length (flatten reduced-rhs)) (value-of lhs limits +inf.0))) ;; enforce depth bound
                 (backtrack return (cdr constraints) (cons (list lhs reduced-rhs) x) length-bound)
                 (return)))] ;;otherwise add and recurse
          
          ;; else we must search
          [ #t  (let* ((to-define (first (append undefined-rhs undefined-lhs))))
                  (stream-for-each (lambda (v) 
                                     (if SHOW-BACKTRACKING
                                         (displayn (string-repeat "\t" (length x)) ; tab to show progress
                                                   "Trying " to-define "=" v "\t with defines " x ))
                                     (call/cc (lambda (ret) (backtrack ret constraints (cons (list to-define v) x) length-bound)))
                                     null ;; must return a value or else scheme goes nuts
                                     )
                                   (stream-filter (lambda (r) (and (check-unique to-define r x)
                                                                   (<= (length (flatten r)) (value-of to-define limits +inf.0))))
                                                  ; holy crap this order matters a lot to uniquenss -- must be reduced before we
                                                  ; check uniqueness
                                                  (stream-filter (if REQUIRE-REDUCED irreducible (lambda (x) #t))
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
                           (displayn "# Trying " to-define " = " v "\t" (length (flatten v)) "\t found\t" found-count)
                           (flush-output-port (current-output-port))))
                     
                     (call/cc (lambda (exit) 
                                (backtrack exit 
                                           constraints 
                                           (cons (list to-define v) defines) 
                                           (length (flatten v)) ; nothing can be longer than the first define in order to ensure efficient search
                                           )))))
                 
                 (stream-filter (if REQUIRE-REDUCED irreducible (lambda (x) #t))
                                (stream-skip PARALLEL-SKIP
                                             (stream-drop PARALLEL-START  ;; handle parallel processing
                                                          (enumerate-all MAX-LENGTH COMBINATOR-BASIS)))))

(displaynerr "# Backtrack count: " GLOBAL-BACKTRACK-COUNT)
(displaynerr "# Found count: " found-count " for " (cdr ARGS))


; Done
(flush-output-port (current-output-port))
(exit (> found-count 0) )
