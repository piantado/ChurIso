;;;; ###########################################################################
;;;; ###########################################################################
;;;;
;;;; This library provides common tools for ChurIso's search algorithms
;;;; to search for valid solutions.
;;;;
;;;; Josh Rule -- joshua.s.rule@gmail.com -- 2016 June
;;;;
;;;; ###########################################################################
;;;; ###########################################################################

(library (search utilities)
  (export push-definitions check-constraints display-winner)
 (import (rnrs) (srfi :41); streams
          (combinators) (stp-lib) (evaluation)
          (data))

  ;; This gives us a for loop with the list of things you loop over at the top.
  ;; This is for use with for-each, as it doesn't return the list.
  ;;
  ;; TODO: Hack! This should only be defined once, not once per file.
  (define-syntax for
    (syntax-rules (in)
      ((_ x in y body ...)
       (for-each (lambda (x) body ...)
                 y))))

  
  (define (can-check-constraint constraint defines)
    (let* ((needed (uniquify (flatten (cdr constraint))))
           (defined (map car defines))
           (defined? (lambda (x) (member? x defined))))
      (all defined? needed)))

  (define (check-constraint constraint defines data params)
    (let* ((type (first constraint))
           (vars (data 'VARIABLES))
           (lhs  (substitute (second constraint) defines vars))
           (rhs  (substitute (third constraint) defines vars)))
      (cond
       [(eqv? type 'normal-form-equal?)     (normal-form-equal?     lhs rhs)]
       [(eqv? type 'normal-form-unequal?)   (normal-form-unequal?   lhs rhs)]
       [(eqv? type 'not-normal-form-equal?) (not-normal-form-equal? lhs rhs)]
       [(eqv? type 'trace-approx-equal?)    (trace-approx-equal?    lhs rhs (params 'PREFIX-DEPTH))]
       [(eqv? type 'normal-form-in?)        (normal-form-in?        lhs rhs)])))

  (define (can-push-definition c defs)
    (let* ((type (first c))
          (lhs (second c))
          (rhs (third c))
          (defined (map first defs))
          (undefined? (lambda (x) (not (member? x defined))))
          (remaining (lambda (xs) (filter undefined? xs))))
      (if (or
           (and (not (list? lhs))
                (not (null? (remaining (list lhs))))
                (null? (remaining rhs)) 
                (equal? type 'normal-form-equal?))
           (and (not (list? rhs))
                (not (null? (remaining (list rhs))))
                (null? (remaining lhs)) 
                (equal? type 'normal-form-equal?)))
          #t
          #f)))

  (define (push-definition c defs data params)
    (let ((type  (first c))
          (lhs  (second c))
          (rhs  (third c))
          (vars (data 'VARIABLES)))
      (if (not (list? lhs)) ;; define lhs as (evaluate rhs) *else* define rhs as (evaluate lhs)
            (let ((reduced-rhs (rebracket (reduce (substitute rhs defs vars)))))
              (if (and (check-unique lhs reduced-rhs defs data params) ;; Enforce uniqueness constraint
                       (is-valid? reduced-rhs)
                       (not (uses-variable? reduced-rhs vars)) ;; cannot push variable names
                       (<= (length (flatten reduced-rhs)) (value-of lhs (data 'LIMITS) +inf.0))) ;; enforce depth bound
                  (cons (list lhs reduced-rhs) defs)
                  defs))
            (let ((reduced-lhs (rebracket (reduce (substitute lhs defs vars)))))
              (if (and (check-unique lhs reduced-lhs defs data params) ;; Enforce uniqueness constraint
                       (is-valid? reduced-lhs)
                       (not (uses-variable? reduced-lhs vars)) ;; cannot push variable names
                       (<= (length (flatten reduced-lhs)) (value-of rhs (data 'LIMITS) +inf.0))) ;; enforce depth bound
                  (cons (list rhs reduced-lhs) defs)
                  defs)))))

  (define (report label x)
    (displayn label " " x)
    x)

  (define (push-definitions cs defs data params)
    (if (null? cs)
        defs
        (let ((c (car cs))
              (cs+ (cdr cs)))
          (if (can-push-definition c defs)
              (push-definitions cs+ (push-definition c defs data params) data params)
              (push-definitions cs+ defs data params)))))

  (define (bad-defs defs data params)
    (let loop ((ds defs))
      (if (null? ds)
          #f
          (let* ((d (car ds));; get the current symbol
                 (name (first d))
                 (val (second d))
                 (vars (data 'VARIABLES))
                 (other-defs (filter (lambda (x) (not (equal? name (first x)))) defs)))
            (if
             (and
              (check-unique name val other-defs data params) ;; Enforce uniqueness constraint
              (is-valid? val) ;; ensure definition is valid
              (not (uses-variable? val vars)) ;; cannot push variable names
              (<= (length* val) (value-of name (data 'LIMITS) +inf.0))) ;; enforce depth bound
             (loop (cdr ds))
             #t)))))
  
  ;; check as many constraints as possible given what's defined
  (define (check-constraints cs defs data params)
    (if (bad-defs defs data params) ;; if the definitions violate length constraints, etc. we need to try something else
        #f
        (if (null? cs)
            null
            (let ((c (car cs))
                  (cs+ (cdr cs)))
              (if (can-check-constraint c defs)
                  (if (check-constraint c defs data params)
                      (begin
                        (check-constraints cs+ defs data params))
                      #f)
                  cs)))))

  (define (display-winner x params data)
    ;; gather the lengths of all components
    (define lengths (map (lambda (y) (length*
                                      (substitute y
                                                  (params 'DEFINED-COMBINATORS)
                                                  (data 'VARIABLES))))
                         (map second x)))

    ;; does this hypothesis "cheat" by exiting early, etc?
    (define valid #t)

    ;; count running time for the constraints
    (define running-time
      (let ((start-count (get-reduction-count)))
        (for c in (data 'CONSTRAINTS)
             (let* ((lhs-subbed (substitute (substitute (second c) x (data 'VARIABLES)) (params 'DEFINED-COMBINATORS) (data 'VARIABLES)))
                    (rhs-subbed (substitute (substitute (third c) x (data 'VARIABLES)) (params 'DEFINED-COMBINATORS) (data 'VARIABLES)))
                    (a          (reduce lhs-subbed))  ; reduce lhs and rhs, can give invalid reductions
                    (b          (reduce lhs-subbed))) ; (e.g. reduced length is too long)
               ;; We need to catch and penalize invalid reductions
               (if (or (not (is-valid? a))
                       (not (is-valid? b))
                       (> (length* a) (params 'MAX-LENGTH))
                       (> (length* b) (params 'MAX-LENGTH)))
                   (set! valid #f))))
        (- (get-reduction-count) start-count)))

    ;; penalize the ones that cheat the search
    (set! running-time (+ (if valid 0 1000000000) running-time))

    ;; Display the total length and running time
    (displayn "Solution: " (params 'FOUND-COUNT) ", Longest Defn: " (apply max lengths) ", total length: "  (apply + lengths) ", No. Reductions: " running-time ", ??: " (* running-time (apply + lengths)) ", Total Backtracks: " (params 'GLOBAL-BACKTRACK-COUNT))

    ;; Display the actual values in full basis
    (displayn "# ---------- In search basis ----------")
    (for xi in x
         (displayn (first xi) " := " (second xi) ))
    
    ;; Display the actual values in SK basis
    (displayn "# ---------- In SK basis ----------")
    (for xi in x
         (displayn (substitute (first xi) (params 'DEFINED-COMBINATORS) (data 'VARIABLES)) " := " (substitute (second xi) (params 'DEFINED-COMBINATORS) (data 'VARIABLES)) ))

    ;; print the show: what you compute, the outcome, and a list of things its equal to
    (displayn "# Showing:")
    (for hi in (data 'SHOWS)
         (let ((rho (reduce (substitute hi x (data 'VARIABLES)))))
           (displayn hi " -> " rho
                     "\twhich equals " (map first (filter (lambda (a) ((params 'are-combinators-equal?) (second a) rho )) x)) )))

    (displayn (string-repeat "#" 79) (params 'EOR)) ; end in #\0 so we can sort -z by multiple lines

    (flush-output-port (current-output-port))

    ;; kill everything if we've found too many
    (if (> (params 'FOUND-COUNT) (params 'MAX-FIND))
        (begin
          (displaynerr "# Terminating from exceeding MAX-FIND")
          (exit 0))
        (params 'set (list 'FOUND-COUNT (+ (params 'FOUND-COUNT) 1)))))

) ;; end library
