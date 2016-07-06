;;;; ###########################################################################
;;;; ###########################################################################
;;;;
;;;; This library takes a ChurIso problem and uses a backtracking algorithm
;;;; to search for valid solutions. 
;;;;
;;;; Steve Piantadosi -- spiantado@gmail.com -- 2016 June
;;;;
;;;; ###########################################################################
;;;; ###########################################################################

(library (search backtrack)
  (export backtrack)
  (import (rnrs) (srfi :41); streams
          (combinators) (stp-lib) (evaluation) (data) (search utilities))

  ;; convert s into a stream that has an offset of start
  ;; and skips skip
  (define (stream-skip skip s)
    (if (stream-null? s)
        stream-null
      (stream-cons (stream-car s)  
                   (stream-skip skip (stream-drop (+ skip 1) s)))))
  
  ;; now constraints can contain lists, where the second element gives the max length* of the combinator we map to
  (define (backtrack is-outer return myconstraints x my-complexity-bound params data)
                                        ; is-outer tells us if we're an outermost (first) constraint, where we may have to skip with PARALLEL variables
                                        ; return is a call/cc for backtracking
                                        ; constraints encodes the current constraints to be defined
                                        ; x is a list of things that are defined (variable value), an assoc list

    (params 'set (list 'GLOBAL-BACKTRACK-COUNT (+ (params 'GLOBAL-BACKTRACK-COUNT) 1)))
    
    (if (null? myconstraints)
        (display-winner x params data) ;; good news!
        (let* ((c           (car myconstraints))
               (constraint-type (first c))
               (lhs         (second c))
               (rhs         (third c))
               (vars        (data 'VARIABLES))
               (in-x?       (lambda (a) (assoc a x)))
               (definable   (lambda (a) (and (not (in-x? a)) ; ;what can we define? Must not be defined (not in x) and not be a variable
                                             (not (is-variable? a vars)))))
               (undefined-lhs (filter definable (flatten lhs)))
               (undefined-rhs (filter definable (flatten rhs)))
               (initial-found-count (params 'FOUND-COUNT))
               (prefix-depth (params 'PREFIX-DEPTH)))
          (cond           
           ;; if we have defined everything
           ;; NOTE: We could write with eval, but that might make fanciness harder later
           [(and (null? undefined-lhs) (null? undefined-rhs))
            (if (cond [(eqv? constraint-type 'normal-form-equal?)     (normal-form-equal?     (substitute lhs x vars) (substitute rhs x vars))]
                      [(eqv? constraint-type 'normal-form-unequal?)   (normal-form-unequal?   (substitute lhs x vars) (substitute rhs x vars))]
                      [(eqv? constraint-type 'not-normal-form-equal?) (not-normal-form-equal? (substitute lhs x vars) (substitute rhs x vars))]
                      [(eqv? constraint-type 'trace-approx-equal?)    (trace-approx-equal?    (substitute lhs x vars) (substitute rhs x vars) prefix-depth)]
                      [(eqv? constraint-type 'normal-form-in?)        (normal-form-in?        (substitute lhs x vars) (substitute rhs x vars))]
                      )
                                        ; If we satisfy this constraint, no complexity penalty
                (backtrack #f return (cdr myconstraints) x my-complexity-bound params data)
                (return))]
           
                                        ; Push a constraint <-
           [(and (null? undefined-rhs) 
                 (not (list? lhs))
                 (equal? constraint-type 'normal-form-equal?))
            (let ((reduced-rhs (rebracket (reduce (substitute rhs x vars)))))
                                        ;(displayn "PUSHING " lhs reduced-rhs)
              (if (and (check-unique lhs reduced-rhs x data params) ;; Enforce uniqueness constraint
                       (is-valid? reduced-rhs)
                       (not (uses-variable? reduced-rhs vars)) ;; cannot push variable names
                       (<= (length (flatten reduced-rhs)) (value-of lhs (data 'LIMITS) +inf.0))) ;; enforce depth bound
                  (backtrack #f return (cdr myconstraints) (cons (list lhs reduced-rhs) x) my-complexity-bound params data)
                  (return)))] ;;otherwise add and recurse
           
           
                                        ; Push a constraint ->
           [(and (null? undefined-lhs) 
                 (not (list? rhs))
                 (equal? constraint-type 'normal-form-equal?))
            (let ((reduced-lhs (rebracket (reduce (substitute lhs x vars)))))
              (if (and (check-unique lhs reduced-lhs x data params) ;; Enforce uniqueness constraint
                       (is-valid? reduced-lhs)
                       (not (uses-variable? reduced-lhs vars)) ;; cannot push variable names
                       (<= (length (flatten reduced-lhs)) (value-of rhs (data 'LIMITS) +inf.0))) ;; enforce depth bound
                  (backtrack #f return (cdr myconstraints) (cons (list rhs reduced-lhs) x) my-complexity-bound params data)
                  (return)))] ;;otherwise add and recurse
           
           ;; else we must search
           [ #t  (let* ((to-define (first (append undefined-rhs undefined-lhs))))
                   (stream-for-each (lambda (v) 
                                        ; Display if we should
                                      (if (or (and is-outer (params 'DISPLAY-INCREMENTAL))
                                              (params 'SHOW-BACKTRACKING))
                                          (displaynerr "#" (string-repeat "\t" (length x))  
                                                       "Trying " to-define " = " v "\t with length " (length* v) 
                                                       ".\t Found\t" (params 'FOUND-COUNT) ". Next bound is " my-complexity-bound))
                                      
                                        ; If we are too long, since we assume in-order generation, we can return
                                      (if (>= (length (flatten v)) (value-of to-define (data 'LIMITS) +inf.0))
                                          (return))
                                      
                                        ; else assign and recurse
                                      (call/cc (lambda (ret) (backtrack #f
                                                                        ret 
                                                                        myconstraints 
                                                                        (cons (list to-define v) x) 
                                                                        (- my-complexity-bound (length* v))
                                                                        params
                                                                        data)))
                                      null) ;; must return a friendly or else scheme goes nuts
                                    (stream-filter (lambda (r) (check-unique to-define r x data params))
                                        ; This order matters a lot to uniqueness -- must be reduced before we
                                        ; check uniqueness
                                                   (stream-filter (cond ((eqv? (params 'COMBINATOR-FILTER) 'normal) is-normal-form?)
                                                                        ((eqv? (params 'COMBINATOR-FILTER) 'compressed) is-normal-form-or-shorter?)
                                                                        ((eqv? (params 'COMBINATOR-FILTER) 'none)  (lambda args #t)))
                                                                  (stream-skip (if is-outer (params 'PARALLEL-SKIP) 0)
                                                                               (stream-drop (if is-outer (params 'PARALLEL-START)  0) ;; handle parallel processing
                                                                                            (enumerate-all (- my-complexity-bound (* 1 (data 'COMPLEXITY)))
                                                                                                           (params 'COMBINATOR-BASIS)))))))  )]
           ))))
  
  ) ;; end library
