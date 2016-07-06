;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;;
;;;; This library processes data files for use throughout ChurIso.
;;;;
;;;; Josh Rule -- joshua.s.rule@gmail.com -- 2016 June
;;;;
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(library (data)
  (export is-variable? uses-variable? substitute read-data-from-file check-unique)
  (import (rnrs) (stp-lib) (data parser) (evaluation)
          (data randomize) (data vertex-cover) (rnrs hashtables (6)))

  (define (read-data-from-file file sort)
    (let* ((ht (make-eqv-hashtable))
           (input (get-input file))
           (data
            (case-lambda
             (() ht)
             ((param) (hashtable-ref ht param 'NA-DATA))
             ((cmd . kvs) (case cmd
                            ((set) (for-each (lambda (kv)
                                               (hashtable-set! ht
                                                               (first kv)
                                                               (second kv)))
                                             kvs))
                            (else (displayn "what do you mean by " cmd "?")))))))
      (data 'set
            (list 'SHOWS     (make-shows input))
            (list 'LIMITS    (make-limits input))
            (list 'VARIABLES (make-variables input))
            (list 'UNIQUES   (make-uniques input))
            (list 'DEFINES   (make-defines input)))
      (add-constraints data input sort)
      data))

  (define (show-data? x) (eqv? (first x) 'show))
  (define (limit-data? x) (eqv? (first x) 'limit))
  (define (variable-data? x) (eqv? (first x) 'variable))
  (define (unique-data? x) (eqv? (first x) 'unique))
  (define (constraint-data? x) (eqv? (first x) 'constrain))
  (define (define-data? x) (eqv? (first x) 'define))

  (define (make-shows input)     (map second (filter show-data? input)))
  (define (make-limits input)    (map cdr (filter limit-data? input)))
  (define (make-uniques input)   (map cdr (filter unique-data? input)))
  (define (make-variables input) (apply append (map cdr (filter variable-data? input))))
  ;; reduce so "showing" works right for things with extra parens
  (define (make-defines input)   (map (lambda (ai) (list (first ai) (reduce (second ai))))
                                      (map cdr    (filter define-data? input))))
  (define (make-raw-constraints input) (map cdr (filter constraint-data? input)))

  (define (add-constraints data input sort)
    (let* ((constraints (make-raw-constraints input))
           (f (case sort
                    ((random) random-sort)
                    ((vertex-cover) vertex-sort)
                    ((none) no-sort))))
      (reorder-constraints f constraints data)))

  (define (reorder-constraints f constraints data)
    (let* ((defines (data 'DEFINES))
           (vars (data 'VARIABLES))
           (complexity1 (compute-complexity constraints defines vars))
           (xyz (f constraints defines data))
           (complexity2 (compute-complexity (data 'CONSTRAINTS) defines vars)))
      (begin
        (displaynerr "# Initial order is " complexity1)
        (displaynerr "# New order is " complexity2)
        (data 'set (list 'COMPLEXITY complexity2)))))

  (define (random-sort constraints defines data)
    (displaynerr "# Optimizing constraints with random permutations")
    (data 'set (list
                'CONSTRAINTS
                (random-optimize-constraints compute-complexity
                                             constraints
                                             defines))))

  (define (vertex-sort constraints defines data)
    (displaynerr "# Optimizing constraints with vertex-cover")
    (let* ((symbols (list-symbols (map cdr constraints)))
           (graph (make-graph constraints))
           (best-of-all-covers (choose-best-cover (make-all-vertex-covers graph)))
           (good-ordering (order-constraints constraints
                                             defines
                                             best-of-all-covers)))
      (data 'set
            (list 'CONSTRAINTS good-ordering)
            (list 'SYMBOLS symbols)
            (list 'GRAPH graph)
            (list 'COVER best-of-all-covers))))

  (define (no-sort constraints defines data)
    (displaynerr "# Not optimizing constraints")
    (data 'set (list 'CONSTRAINTS constraints)))

  ;; take a set of constraints and compute the complexity
  ;; of our search. It will be O(N^k), and this returns k
  ;; This gets used by a constraint-order-randomizer to pick
  ;; a good order to search in
  (define (compute-complexity constraints defines vars)
    (if (null? constraints)
        0
        (let* ((c          (car constraints))
               (constraint-type (first c))
               (lhs         (second c))
               (rhs         (third c))
               (in-x?       (lambda (a) (assoc a defines)))
               (definable   (lambda (a) (and (not (in-x? a)) ; ;what can we define? Must not be defined (not in x) and not be a variable
                                             (not (is-variable? a vars)))))
               (undefined-lhs (filter definable (flatten lhs)))
               (undefined-rhs (filter definable (flatten rhs)))
               )
          (cond [(and (null? undefined-lhs) (null? undefined-rhs)) ; everything defined
                 (compute-complexity (cdr constraints) defines vars)]
                [(and (null? undefined-rhs)                        ; push constraint <-
                      (not (list? lhs))
                      (equal? constraint-type 'normal-form-equal?))
                 (compute-complexity (cdr constraints) (cons (list lhs 'dummy) defines) vars)]
                [(and (null? undefined-lhs)                        ; puch constraint ->
                      (not (list? rhs))
                      (equal? constraint-type 'normal-form-equal?))
                 (compute-complexity (cdr constraints) (cons (list rhs 'dummy) defines) vars)]
                [ #t (+ 1 (compute-complexity constraints (cons (list (first (append undefined-rhs undefined-lhs)) 'dummy) defines) vars))] ; we have to search
                ))))

  (define (is-variable? x vars) (member? x vars))

  (define (uses-variable? a vars)
    (any (lambda (x) (is-variable? x vars))
         (flatten a)))

  (define (substitute obs defined vars)
    (sub-f (lambda (a) (cond [(is-variable? a vars) a] ;; variables are just themselves
                             [(assoc a defined) (second (assoc a defined))] ;; look up if its in x
                             [ #t a ] ;; otherwise we assume a "quoted" variable. TODO: THIS SHOULD CHANGE
                             ))
           obs))

  ;; look in uniques to see if tok can have value val given the values in x
  (define (check-unique tok val ds data params)
    (let loop ((uniques (data 'UNIQUES)))
      (if (null? uniques)
          #t ;; it's good!
          (let ((l (car uniques)))
            (and (if (member? tok l)
                     ;; look up each element of l and check that it's value in x is not val
                     (all (lambda (k) (not (are-combinators-equal? (value-of k ds 'NA-VALUE) val params))) l)
                     #t)
               (loop (cdr uniques)))))))

  ) ;; end library
