;;;; ###########################################################################
;;;; ###########################################################################
;;;;
;;;; This library takes a ChurIso problem and uses a backtracking algorithm
;;;; to search for valid solutions. It uses a priority queue to represent
;;;; unexplored branches of the tree, so search is more efficient than naive
;;;; backtracking, favoring short branches close to a solution over others.
;;;;
;;;; Josh Rule -- joshua.s.rule@gmail.com -- 2016 June
;;;;
;;;; ###########################################################################
;;;; ###########################################################################

(library (search pq-search)
  (export pq-search)
  (import (rnrs) (stp-lib) (pfds psqs) (srfi :41) (combinators)
          (data vertex-cover) (evaluation) (search utilities))

  ;;; priority queue tools
  
  (define (compare a b)
    (or
     ;; if we're looking at two numbers, choose the smaller number
     (and (number? a) (number? b) (< a b))
     ;; if we're looking at a number and a list, choose the number
     (and (number? a) (list? b))
     ;; if we're looking at two lists/trees, choose the lesser tree
     (and (list? a) (list? b) (tree<? a b))))

  (define (tree<? a b)
    (let* ((list->string (lambda (xs) (apply string-append (map symbol->string (flatten xs))))))
      (cond
       [(equal? a b) #f]
       ;; if b is null, then a is either longer or the same (also null), so an immediate false
       [(null? b) #f]
       ;; if a is shorter than b, then #t (includes "a is null and b is not null" base case)
       [(< (length* a) (length* b)) #t]
       ;; if a is longer than b, so an immediate false
       [(> (length* a) (length* b)) #f]
       ;; else they're the same length, but if a is lexicographically before b, then #t
       [(string<? (list->string a) (list->string b)) #t]
       ;; else if the string is greater, an immediate false
       [(string>? (list->string a) (list->string b)) #f]
       ;; else if the left-branch is lesser 
       [(tree<? (first (rebracket a)) (first (rebracket b))) #t]
       ;; else if the right-branch is lesser
       [(and (equal? (first (rebracket a)) (first (rebracket b))) (tree<? (second (rebracket a)) (second (rebracket b)))) #t]
       ;; otherwise, they must be equal!
       [else #f])))

  ;; how do we know if two hypotheses/keys are equivalent?
  (define (key<? k1 k2)
    (let ((shorter-constraints (< (length (second k1))
                                  (length (second k2))))
          (k1-content (map second (first k1)))
          (k2-content (map second (first k2))))
      (let loop ((ks1 k1-content)
                 (ks2 k2-content))
        (if (or (null? ks1) (null? ks2))
            #f
            (if (equal? (car ks1) (car ks2))
                (loop (cdr ks1) (cdr ks2))
                (or (compare (car ks1) (car ks2))
                    shorter-constraints))))))

  ;; priorities are based on constraints remaining and number of
  ;; symbols, so they are pairs (lists of length two, not cons pairs)
  ;; these should be (#sym, #constraints)
  (define (priority<? p1 p2)
    (cond
     [(< (first p1) (first p2)) #t]
     [(< (first p2) (first p1)) #f]
     [(< (second p1) (second p2)) #t]
     [else #f]))

  ;; add many key-priority pairs to a queue
  (define (enqueue q kps)
    (if (null? kps)
        q
        (enqueue (psq-set q (first (car kps)) (second (car kps))) (cdr kps))))
  
  ;; remove many keys from queue
  (define (dequeue queue keys)
    (fold-right (lambda (key acc)
                  (psq-delete acc key))
                queue
                keys))

  ;; compute a priority for some hypothesis/key and the current constraints
  (define (compute-priority key)
    (list (fold-right (lambda (x acc) (+ acc (if (list? x) (length* x) x)))
                      0
                      (map second (first key)))
          (length (second key))))

;;; the actual backtracking algorithms

  ;; give back minimally more specific but equally complex nodes
  (define (increase-specificity h basis constraints)
    (let* ((specified (filter (lambda (x) (list? (second x))) h))
           (unspecified (filter (lambda (x) (number? (second x))) h))
           (to-specify (car unspecified))
           (name (first to-specify))
           (n (second to-specify))
           (still-unspecified (cdr unspecified))
           (possible-specs (map list (stream->list (enumerate-at n basis))))
           (just-specified (map (lambda (x) (list name x)) possible-specs))
           (new-hypotheses (inserts specified just-specified still-unspecified))
           (new-keys (map (lambda (x) (list x constraints)) new-hypotheses))
           (new-kps (map (lambda (x) (list x (compute-priority x))) new-keys)))
      new-kps))

  ;; give back equally specific but minimally more complex nodes
  (define (increase-complexity h upper-bound basis constraints)
    (if (>= (length* h) upper-bound)
        '()
        (let* ((specified (filter (lambda (x) (list? (second x))) h))
               (unspecified (filter (lambda (x) (number? (second x))) h))
               (staying (take (- (length specified) 1) specified))
               (changing (last specified))
               (new-lists (map (lambda (x) (append (second changing) (list x))) basis))
               (new-combinators (stream->list (stream-concat (list->stream (map enumerate-trees new-lists)))))
               (new-defines (map (lambda (x) (list (first changing) x)) new-combinators))
               (new-hypotheses (inserts staying new-defines unspecified))
               (new-keys (map (lambda (x) (list x constraints)) new-hypotheses))
               (new-kps (map (lambda (x) (list x (compute-priority x))) new-keys)))
          new-kps)))

  ;; remove from queue entries sharing an identical vertex cover as hypothesis
  ;; TODO: this is hacky! Priority Queues shouldn't be dumped out like this
  (define (drop-covered-keys hypothesis queue cover-size max-priority)
    (let* ((entries (psq-at-most queue max-priority))
           (keys (map car entries))
           (same-cover? (lambda (x) (equal? (take cover-size hypothesis)
                                            (take cover-size (first x))))))
      (dequeue queue (filter same-cover? keys))))

  (define (update-hypothesis hypothesis defined)
    (if (null? hypothesis)
        null
        (let* ((defined-symbols (map first defined))
               (symbol-under-question (first (first hypothesis)))
               (definition (value-of symbol-under-question defined #f)))
          (cons (if definition
                    (list symbol-under-question definition)
                    (first hypothesis))
                (update-hypothesis (cdr hypothesis) defined)))))

  (define (report name val)
    (displayn name " " val)
    val)
  
  ;; assumes an optimal ordering of the symbols and the constraints
  (define (pq-search params data)
    (let* ((upper (params 'MAX-LENGTH)) ;; TODO: should we use a different value?
           (defines (data 'DEFINES))
           (symbols (set-difference (data 'SYMBOLS)
                                    (map first defines)))
           (constraints (data 'CONSTRAINTS))
           (max-priority (list upper (length constraints)))
           (basis (params 'COMBINATOR-BASIS))
           (cover (data 'COVER))
           (key (list (map (lambda (s) (list s 1)) symbols) constraints))
           (priority (compute-priority key))
           (queue  (psq-set (make-psq key<? priority<?) key priority)))
      (let loop ((q queue))
        (let* ((key (psq-min q))
               (hypothesis (first key))
               (cs (second key))
               (priority (psq-ref q key))
               (q-less-h (psq-delete-min q))
               (pre-defined (append (filter (lambda (x) (list? (second x))) hypothesis) defines))
               (defined (push-definitions cs pre-defined data params))
               (cs+ (check-constraints cs defined data params))
               (h+ (update-hypothesis hypothesis defined))
               (q+ (cond ;; 'cond' here is semantically awful, but effective
                    [(and (list? cs+) (null? cs+)) ;; found a solution!
                     (display-winner defined params data)
                     (drop-covered-keys hypothesis q-less-h (length cover) max-priority)]
                    [(list? cs+)  ;; hypothesis is okay but constraints left -> increase specificity
                     (enqueue q-less-h (increase-specificity h+ basis cs+))]
                    [else ;; hypothesis failed -> increase complexity
                     (enqueue q-less-h (increase-complexity hypothesis upper basis constraints))] )))
          (if (psq-empty? q+)
              (exit 0)
              (loop q+))))))

  ) ;; end library
