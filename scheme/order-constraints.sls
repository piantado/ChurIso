;;;; This library takes the list of constraints for some problem and
;;;; reorders the constraints to improve learning.
(library
 (order-constraints)
 (export constraint? list-constraints list-symbols  make-dependency-graph
         adjacency-to-ge-graph make-vertex-cover choose-best-of-n-covers
         all-vertex-covers choose-best-of-all-covers)
 ;; (export find-vertex-cover reorder-constraints)
 (import (rnrs) (stp-lib) (parser))

 (define (constraint? xs)
   (eqv? (first xs) 'constrain))

 (define (list-constraints inputs)
   (map cdr
       (filter constraint? inputs)))

 (define (list-symbols constraints)
   (uniquify (flatten (map cdr constraints))))

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
          (symbol-depends-on (lambda (sym)
                            (uniquify
                             (remove sym
                                     (flatten
                                      (filter (lambda (cd) (contains? cd sym))
                                              dependencies))))))
          (symbol-dependency-pair (lambda (symbol)
                                    (cons symbol
                                          (symbol-depends-on symbol)))))
     (map symbol-dependency-pair symbols)))

 (define (adjacency-to-ge-graph graph)
  (let* ((symbols (map car graph))
         (cons-with (lambda (x)
                      (lambda (y)
                        (cons x y))))
         (symbol-edges (lambda (pr)
                         (map (cons-with (car pr))
                              (cdr pr))))
         (list-of-lists-of-edges (map symbol-edges graph))
         (list-of-edges (fold-right append '() list-of-lists-of-edges))
         (flip-pr (lambda (x) (cons (cdr x) (car x))))
         (same-edge? (lambda (x)
                       (lambda (y)
                         (or (equal? x y)
                             (equal? x (flip-pr y))))))
         (builder (lambda (x acc) (if (any (same-edge? x) acc)
                                      acc
                                      (cons x acc))))
         (edges (list (fold-right builder '() list-of-edges))))
    (cons symbols edges)))

 (define (choose-best-of-n-covers n g)
   (choose-best-of-n-covers-helper
    (list (car g) '())
    (map (lambda (x) (make-vertex-cover g)) (range n))))

 (define (choose-best-of-n-covers-helper c cs)
   (if (null? cs)
       c
       (if (< (length (car (car cs))) (length (car c)))
           (choose-best-of-n-covers-helper (car cs) (cdr cs))
           (choose-best-of-n-covers-helper c (cdr cs)))))

 (define (make-vertex-cover g)
   (make-vertex-cover-helper (first g) (second g) '()))

 (define (make-vertex-cover-helper ns es c)
   (if (null? es)
       (list c (set-difference ns c))
       (let* ((e (random-element es))
              (u (car e))
              (v (cdr e))
              (new-c (uniquify (cons u (cons v c))))
              (new-es (filter (lambda  (x) (not (or (equal? (car x) u)
                                                    (equal? (car x) v)
                                                    (equal? (cdr x) u)
                                                    (equal? (cdr x) v)))) es)))
         (make-vertex-cover-helper ns new-es new-c))))

 (define (all-vertex-covers g)
   (all-vertex-covers-helper '() (car g) (car (cdr g))))

 (define (all-vertex-covers-helper c ns es)
   (if (null? ns)
       (if (null? es)
           (list c)
           '())
       (let* ((u (car ns))
              (new-c (cons u c))
              (new-es (filter (lambda (e) (not (or (equal? (car e) u)
                                                   (equal? (cdr e) u)))) es)))
         (append (all-vertex-covers-helper c (cdr ns) es)
                 (all-vertex-covers-helper new-c (cdr ns) new-es)))))

 (define (choose-best-of-all-covers g)
   (let* ((nodes (car g))
          (covered (choose-best-of-all-covers-helper
                       nodes
                       (all-vertex-covers g)))
          (uncovered (set-difference nodes covered)))
   (list covered uncovered)))

 (define (choose-best-of-all-covers-helper c cs)
   (if (null? cs)
       c
       (if (< (length (car cs)) (length c))
           (choose-best-of-all-covers-helper (car cs) (cdr cs))
           (choose-best-of-all-covers-helper c (cdr cs)))))


 )
