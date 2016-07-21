;;;; ###########################################################################
;;;; ###########################################################################
;;;;
;;;; This library searches for valid solutions to a ChurIso problem via
;;;; backtracking. The backtracker uses a priority queue to represent unexplored
;;;; branches of the tree, so search is more efficient than naive backtracking,
;;;; favoring short branches close to a solution over others.
;;;;
;;;; Josh Rule -- joshua.s.rule@gmail.com -- 2016 June
;;;;
;;;; ###########################################################################
;;;; ###########################################################################

(library (search pq-search)
  (export pq-search list2string)
  (import (rnrs) (stp-lib) (pfds psqs) (srfi :41) (combinators)
          (data vertex-cover) (evaluation) (search utilities))

  ;;; priority queue tools

  (define (list2string x)
    (cond
     [(symbol? x) (symbol->string x)]
     [(number? x) (number->string x)]
     [(and (list? x) (null? x)) "()"]
     [(list? x) (string-append "(" (join " " (map list2string x)) ")")]
     [else "ERROR"]))

  ;; how do we know if two hypotheses/keys are equivalent? we convert
  ;; them to strings and ask which comes first
  (define (key<? k1 k2)
    (string<? (list2string k1) (list2string k2)))

  ;; priorities are pairs (lists of length two, not cons pairs)
  ;; (#sub-combinators, #constraints-remaining)
  (define (priority<? p1 p2)
    (< (fold-left + 0 p1) (fold-left + 0 p2)))

  ;; add many key-priority pairs to a queue
  (define (enqueue queue kps)
    (fold-left (lambda (q kp) (psq-set q (first kp) (second kp))) queue kps))

  ;; remove many keys from queue
  (define (dequeue queue keys)
    (fold-left (lambda (q key) (psq-delete q key)) queue keys))

  ;; compute a priority for some hypothesis/key and the current constraints
  (define (compute-priority key)
    (list (third key) (length (second key))))

  ;;; the search algorithm

  (define (expand key data params)
    (let*-values
        (((h) (first key))
         ((cs) (second key))
         ((n-symbols) (third key))
         ((specified unspecified) (partition (lambda (x) (not (number? (second x)))) h))
         ((to-specify) (car unspecified))
         ((name) (first to-specify))
         ((n) (second to-specify))
         ((still-unspecified) (cdr unspecified))
         ((basis) (params 'COMBINATOR-BASIS))
         ;; add more complex *and* more specific hypotheses simultaneously
         ((possible-specs) (cons (+ n 1) (stream->list (enumerate-at n basis))))
         ((just-specified) (map (lambda (x) (list name x)) possible-specs))
         ((push-constraints) (lambda (h) (propagate-definitions h cs data params)))
         ((candidate-hypotheses) (map push-constraints (inserts specified just-specified still-unspecified)))
         ((candidate-lengths) (cons (+ n-symbols 1) (repeat n-symbols (- (length possible-specs) 1))))
         ((new-keys) (fold-left
                      (lambda (acc x y) (let ((cs+ (check-constraints cs x data params)))
                                          (params 'set (list 'GLOBAL-BACKTRACK-COUNT (+ (params 'GLOBAL-BACKTRACK-COUNT) 1)))
                                          (cond
                                           [(and cs+ (null? cs+)) (display-winner x params data) acc]
                                           [cs+ (cons (list x cs+ y) acc)]
                                           [else acc])))
                      null
                      candidate-hypotheses
                      candidate-lengths))
         ((new-kps) (map (lambda (x) (list x (compute-priority x))) new-keys)))
      new-kps))

  (define (propagate-definitions h cs data params)
    (let* ((pre-defined (append (filter (lambda (x) (not (number? (second x)))) h)
                                (data 'DEFINES)))
           (defined (push-definitions cs pre-defined data params))
           (f (lambda (def) (let ((val (value-of (first def) defined #f)))
                              (if val (list (first def) val) def)))))
      (map f h)))

  (define (pq-search params data)
    (let* ((symbols (set-difference (data 'SYMBOLS) ;; what do we actually need to define?
                                    (set-union (map first (data 'DEFINES))
                                               (data 'VARIABLES))))
           (constraints (data 'CONSTRAINTS))
           (key (list (map (lambda (s) (list s 1)) symbols) constraints (length symbols)))
           (priority (compute-priority key))
           (queue  (psq-set (make-psq key<? priority<?) key priority)))
      (let loop ((q queue))
        (let* ((key (psq-min q))
               ;;(priority (psq-ref q key))
               ;; remove best hypothesis and add new hypotheses which contingently satisfy the constraints
               (q+ (enqueue (psq-delete-min q) (expand key data params))))
          ;;(displayn priority " " (first key))
          (if (psq-empty? q+)
              null
              (loop q+)))))) ;; recurse

  ) ;; end library
