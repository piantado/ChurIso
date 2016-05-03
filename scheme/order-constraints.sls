;;;; This library takes the list of constraints for some problem and
;;;; reorders the constraints to improve learning.
(library 
 (order-constraints)
 (export constraint? list-constraints list-symbols  make-dependency-graph)
 ;; (export find-vertex-cover reorder-constraints)
 (import (rnrs) (stp-lib) (parser))

 (define (constraint? xs)
   (eqv? (first xs) 'constrain))
 
 (define (list-constraints inputs)
   (map cdr
       (filter constraint? inputs)))
 
 (define (list-symbols constraints)
   (uniquify (flatten (map cdr constraints))))
 
 (define (not-x? x)
   (lambda (y) (not (equal? x y))))
 
 ;; builds the graph of dependencies between symbols:
 ;; - create a unique list of symbols involved in the constraints
 ;; - for each symbol, x (i.e. using map):
 ;;   - make a list of the constraints containing the symbol
 ;;   - Flatten the list and uniquify it. Call it y
 ;;   - leave (cons x y)
 (define (make-dependency-graph input)
   (let* ((constraints (list-constraints input))
          (symbols (list-symbols constraints))
          (constraint-dependencies (lambda (x) (uniquify (flatten (cdr x)))))
          (dependencies (map constraint-dependencies constraints))
          (sym-depends-on (lambda (sym)
                            (uniquify
                             (filter (not-x? sym)
                                     (flatten (filter (lambda (cd) (contains? cd sym))
                                                      dependencies))))))
          (symbol-dependency-pair (lambda (sym) (cons sym (sym-depends-on sym)))))
     (map symbol-dependency-pair symbols)))
 )



