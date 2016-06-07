(library (parameters)
  (export initialize-parameters)
  (import (rnrs) (stp-lib) (rnrs hashtables (6)))

  (define (initialize-parameters ARGS)
    (let* ((ht (make-eqv-hashtable))
           (params
            (case-lambda
             (() ht)
             ((param) (hashtable-ref ht param 'NA-PARAM))
             ((cmd . kvs) (case cmd
                            ((set) (for-each (lambda (kv)
                                               (hashtable-set! ht
                                                               (first kv)
                                                               (second kv)))
                                             kvs))
                            (else (displayn "what do you mean by " cmd "?")))))))
      (params 'set
       ;; where our input comes from
       (list 'DATA-FILE (second ARGS))
       ;; how ChurIso searches in parallel: the START and the SKIP
       ;; for the first defined value. 0,0 runs in a single thread
       (list 'PARALLEL-START (string->number (third ARGS)))
       (list 'PARALLEL-SKIP  (string->number (fourth ARGS)))
       ;; max length of combinators that are allowed
       (list 'MAX-LENGTH     20)
      ;; End of Record, #\nul lets you sort -z with multiline output
       (list 'EOR            #\nul)
      ;; how many solutions should we find, at maximum?
      (list 'MAX-FIND       10000)
      ;; how should we search for them? ('backtrack or 'pq)
      (list 'SEARCH-METHOD 'pq)
      ;; when we say that prefixes must be equal, how deep is the equality?
      (list 'PREFIX-DEPTH   25)
      ;; - 'normal (must be normal form)
      ;; - 'compressed (non-normal forms is fine if shorter than normal form)
      ;; - 'none (all combinators)
      (list 'COMBINATOR-FILTER 'normal)
      ;; How do we enforce uniqueness?
      ;; - normal-form-equal? (normal)
      ;; - trace-approx-equal? (trace)
      ;; - equal? (equal)?
      (list 'COMBINATOR-EQUALITY  'normal)
      (list 'COMBINATOR-BASIS '(S K I B C))
      ;; display the outermost search to show progress?
      (list 'DISPLAY-INCREMENTAL (or (member "--incremental" ARGS)
                                    (member "--verbose" ARGS)))
      ;; show all stages of the backtracking search?
      (list 'SHOW-BACKTRACKING   (member "--verbose" ARGS))
      ;; random, vertex-cover, or none
      (list 'CONSTRAINT-SORT 'vertex-cover)
      ;; how many times have we called backtrack?
      (list 'GLOBAL-BACKTRACK-COUNT 0)
      ;; how many did we find?
      (list 'FOUND-COUNT 0)
      ;; the complexity of defined combinators is measured by
      ;; running them as defined. You search with them as primitives
      ;; (for speed) but measure their complexity via S,K (for
      ;; parsimony). Here, we have defined B and C using cl's
      ;; "tromp" algorithm to minimize the number of S,K
      (list 'DEFINED-COMBINATORS
           '((I (S K K))
             (C (S (K (S (K (S S (K K))) K)) S))
             (B (S (K S) K) ))))
      ;; finally, we return this function
      params))
  
  ) ;; end library
