#!/usr/bin/env scheme-script
#!r6rs

(import
 (rnrs)
 (vicare)
 (stp-lib)
 (parser)
 (order-constraints))


(define all-input (get-input "../domains/boolean.txt"))

(define constraints (list-constraints all-input))

(define symbols (list-symbols constraints))

(define dependency-graph (make-dependency-graph all-input))

(define ge-graph (adjacency-to-ge-graph dependency-graph))

(displaynerr "\ninput\n")
(map displaynerr all-input)

(displaynerr "\nconstraints\n")
(map displaynerr constraints)

(displaynerr "\nsymbols\n")
(map displaynerr symbols)

(displaynerr "\ndependency graph\n")
(map displaynerr dependency-graph)

(displaynerr "\nge graph\n")

(displaynerr ge-graph)

(displaynerr "\nvertex covers\n")
(map displaynerr (map (lambda (x) (make-vertex-cover ge-graph)) (range 15)))

(displaynerr "\nbest vertex covers\n")
(map displaynerr (choose-best-of-n-covers 15 ge-graph))

(displaynerr "\nall vertex covers\n")
(map displaynerr (all-vertex-covers ge-graph))

(displaynerr "\nbest of all vertex covers\n")
(map displaynerr (choose-best-of-all-covers ge-graph))
