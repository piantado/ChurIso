;;;; ###########################################################################
;;;; ###########################################################################
;;;;
;;;; This library takes a ChurIso problem definition and reorders the
;;;; constraints to improve learning. The reordering is based on an (approximate
;;;; or exact) vertex covering of the dependency graph induced by the
;;;; constraints.
;;;;
;;;; Josh Rule -- joshua.s.rule@gmail.com -- 2016 May
;;;;
;;;; ###########################################################################
;;;; ###########################################################################

(library (order-constraints)
  (export make-graph make-vertex-cover make-n-vertex-covers
          make-all-vertex-covers choose-best-cover order-constraints list-symbols
          test-order-constraints)
  (import (rnrs) (stp-lib))

  (define (shares-element e1 e2)
    (not (null? (set-intersection e1 e2))))

  (define (same-edge? e1 e2)
    (or (equal? e1 e2)
        (equal? e1 (reverse e2))))

  (define (constraint? xs)
    (eqv? (first xs) 'constrain))

  (define (list-constraints inputs)
    (map cdr (filter constraint? inputs)))

  (define (list-symbols xs)
    (uniquify (flatten xs)))

  (define (a-before-b a b)
    (let ((a-b  (set-difference a b)))
      (if (null? a-b)
          '()
          (set-union a-b (set-difference b a)))))

  (define (pair-with y xs)
    (map (lambda (x) (list x y)) xs))

  (define (list-pairs xs)
    (if (>= 1 (length xs))
        '()
        (append (pair-with (car xs) (cdr xs))
                (list-pairs (cdr xs)))))

  (define (list-edges cs)
    (let* ((all-edges (apply append (map list-pairs cs)))
           (add-if-new (lambda (y xs) (if (any (lambda (x) (same-edge? x y)) xs)
                                          xs
                                          (cons y xs)))))
      (fold-right add-if-new '() all-edges)))

  (define (make-graph input)
    (let* ((constraints (map cdr (list-constraints input)))
           (nodes (list-symbols constraints))
           (edges (list-edges (map list-symbols constraints))))
      (list nodes edges)))

  (define (make-vertex-cover g)
    (let loop ((es (second g))
               (c '()))
      (if (null? es)
          c
          (let ((e (random-element es)))
            (loop (filter (lambda  (x) (not (shares-element e x))) es)
                  (set-union e c))))))

  (define (make-n-vertex-covers n g)
    (map (lambda (x) (make-vertex-cover g)) (range n)))

  (define (make-all-vertex-covers g)
    (let loop ((c '())
               (ns (first g))
               (es (second g)))
      (cond
       ((and (null? ns) (null? es)) (list c))
       ((and (null? ns) (not (null? es))) '())
       (#t (append (loop c                 ; ignore current node
                         (cdr ns)
                         es)
                   (loop (cons (car ns) c) ; use current node
                         (cdr ns)
                         (filter (lambda (e) (not (member? (car ns) e))) es)))))))

  (define (choose-best-cover covers)
    (random-element (best-by-score covers (map length covers))))

  (define (compute-violations constraints cover)
    (map (lambda (c) (length (a-before-b cover (list-symbols c))))
         constraints))

  (define (compute-costs constraints defines)
    (let ((compute-terms
           ;; You can show via induction that 3|x|-1 is the number of
           ;; terminals in the CL parse tree of x -- JSR
           (lambda (x) (- (* 3 (length (flatten x))) 1)))
          (compute-new-defs
           (lambda (x) (* 2 (length (set-difference (list-symbols x) defines))))))
      (map +
           (map compute-terms (map first constraints))
           (map compute-terms (map second constraints))
           (map compute-new-defs constraints))))

  (define (best-by-violations constraints uncovered)
    (best-by-score constraints (compute-violations constraints uncovered)))

  (define (best-by-costs constraints defines)
    (best-by-score constraints (compute-costs constraints defines)))

  (define (next-constraint constraints defines cover)
    (list (random-element (best-by-costs (best-by-violations constraints
                                                             (set-difference
                                                              cover
                                                              defines))
                                         defines))))

  (define (order-constraints constraints defines cover)
    (let loop ((result '())
               (cs constraints)
               (ds defines))
      (if (null? cs)
          result
          (let ((next (next-constraint cs ds cover)))
            (loop (append result next)
                  (set-difference cs next)
                  (uniquify (append ds (flatten next))))))))

  (define (test-order-constraints all-input)
    (begin
      (define constraints (list-constraints all-input))
      (define symbols (list-symbols (map cdr constraints)))
      (define graph (make-graph all-input))
      (define n-covers 15)
      (define some-covers (make-n-vertex-covers n-covers graph))
      (define best-of-some-covers (choose-best-cover some-covers))
      (define all-covers (make-all-vertex-covers graph))
      (define best-of-all-covers (choose-best-cover all-covers))
      (define ordering (order-constraints (map cdr constraints)
                                          '()
                                          best-of-all-covers))

    (displaynerr "\n#### input ############################################\n")
    (map displaynerr all-input)

    (displaynerr "\n#### constraints ######################################\n")
    (map displaynerr constraints)

    (displaynerr "\n#### symbols ##########################################\n")
    (map displaynerr symbols)

    (displaynerr "\n#### graph nodes ######################################\n")
    (displaynerr (first graph))

    (displaynerr "\n#### graph edges ######################################\n")
    (map displaynerr (second graph))

    (displaynerr "\n#### " n-covers " vertex covers #################################\n")
    (map displaynerr some-covers)

    (displaynerr "\n#### the best of these " n-covers " #############################\n")
    (displaynerr best-of-some-covers)

    (displaynerr "\n#### all vertex covers ################################\n")
    (map displaynerr all-covers)

    (displaynerr "\n#### best of all vertex covers ########################\n")
    (displaynerr best-of-all-covers)

    (displaynerr "\n#### ordering constraints #############################\n")
    (map displaynerr ordering)))
  )
