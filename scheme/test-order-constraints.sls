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

(displaynerr "\ninput\n")
(map displaynerr all-input)

(displaynerr "\nconstraints\n")
(map displaynerr constraints)

(displaynerr "\nsymbols\n")
(map displaynerr symbols)

(displaynerr "\ndependency graph\n")
(map displaynerr dependency-graph)

