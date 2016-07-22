(library
 (stp-lib)
 (export displayn displaynerr null first second third fourth fifth sixth seventh member?
         flatten append apply-append unlist-singleton str-split is-comment-line? is-blank-line? load-file has-value value-of range all any abrange
         string->S-expression drop assert-equal repeat string-repeat mydisplay last length* shuffle
         uniquify nth random-element set-difference minimum maximum set-symmetric-difference range0
         set-union set-intersection best-by-score flip take zip insert inserts sub-f value-of2
         report join
         )
 (import (rnrs) (vicare)) ;  (rnrs io (6))

 ;; #####################################################################################
 ;; #####################################################################################
 ;; Library functions to make scheme a little nicer.
 ;; #####################################################################################
 ;; #####################################################################################

 (define (report do-it? name val)
   (if do-it?
       (begin
         (displayn name " " val)
         val)
       val))

 (define (mydisplay . args)
   (for-each (lambda (a) (display a))
             args)
   (flush-output-port (current-output-port))
   )

 (define (displayn . args)
   (apply mydisplay (append args (list "\n")))
   )


 (define (mydisplayerr . args)
   (for-each (lambda (a) (display a (current-error-port)))
             args)
   (flush-output-port (current-error-port))
   )

 (define (displaynerr . args)
   (apply mydisplayerr (append args (list "\n")))
   )

 (define null '() )

 (define (singleton? lst) (= (length lst) 1))

 (define (first x)  (car x))
 (define (second x) (cadr x))
 (define (third x)  (caddr x))
 (define (fourth x) (cadddr x))
 (define (fifth x)  (cadddr (cdr x)))
 (define (sixth x)  (cadddr (cddr x)))
 (define (seventh x)  (cadddr (cdddr x)))
 (define (nth n xs)
   (if (= n 0)
       (car xs)
       (nth (- n 1) (cdr xs))))

 (define (random-element xs)
   (nth (random (length xs)) xs))

 (define (set-union a b) (uniquify (append a b)))

 (define (set-intersection a b)
   (filter (lambda (x) (member? x a)) b))

 (define (set-difference xs ys)
   (fold-right (lambda (y acc) (remove y acc)) xs ys))

 (define (set-symmetric-difference a b)
   (set-union (set-difference a b)
              (set-difference b a)))

 (define (best-by-score xs scores)
   (let* ((best-score (minimum scores))
          (zipped (map cons scores xs))
          (is-best? (lambda (x) (= (car x) best-score)))
          (best-zipped (filter is-best? zipped))
          (best-xs (map cdr best-zipped)))
     best-xs))

 (define member? member)

 ;; like map, but recurses down lists of lists, applying l
 (define (sub-f f l)
   (cond [(null? l)  null]
         [(list? l)  (map (lambda (li) (sub-f f li)) l)]
         [ #t        (f l) ]))

 (define (drop n x)
   (if (<= n 0)
       x
       (cdr (drop (- n 1) x))))

 (define (flatten x)
   (cond ((null? x) '())
         ((not (pair? x)) (list x))
         (else (append (flatten (car x))
                       (flatten (cdr x))))))
 ;; http://lists.racket-lang.org/users/archive/2009-August/034974.html
 (define (shuffle x)
   (do ((v (list->vector x)) (n (length x) (- n 1)))
       ((zero? n) (vector->list v))
     (let* ((r (random n)) (t (vector-ref v r)))
       (vector-set! v r (vector-ref v (- n 1)))
       (vector-set! v (- n 1) t))))

 (define (flip) (= (random 2) 1))

 ; (define (append . lsts)
 ;   (cond
 ;     ((null? lsts) '())
 ;     ((null? (car lsts)) (apply append (cdr lsts)))
 ;     (else (cons (caar lsts) (apply append (cdar lsts) (cdr lsts))))))

 ;; sometimes we have too many arguments, so this wraps for that
 (define (apply-append lsts)
   (if (null? lsts)
       '()
       (append (car lsts) (apply-append (cdr lsts)))))

 ;; take singleton lists like "((x))"  (arbitrarily deeply embedded) and map to "x"
 (define (unlist-singleton x)
   (cond ((null? x) '())
         ((and (list? x) (null? (cdr x)))  (unlist-singleton (car x)))
         ( #t x)))

 (define (first-satisfying f l)
   (cond ((null? l) #f)
         ((f (car l)) (car l))
         ( #t (first-satisfying f (cdr l)))))

 (define (any f lst)
   (if (null? lst)
       #f
       (or (f (car lst)) (any f (cdr lst)))))

 (define (all f lst)
   (if (null? lst)
       #t
       (and (f (car lst)) (all f (cdr lst)))))

 ;; like length, but counts sublists
 (define (length* x)
   (if (not (list? x))
       1
       (apply + (map length* x))))

 (define (last l)
   (if (null? (cdr l))
       (car l)
       (last (cdr l))))

 (define (depth x)
   (if (list? x)
       (+ 1 (apply max (map depth x)))
       0))

 (define (take n xs)
   (if (= n 0)
       '()
       (cons (car xs) (take (- n 1) (cdr xs)))))

 (define (count f s)
   (if (null? s)
       0
       (+ (if (f (car s)) 1 0)
          (count f (cdr s)))))

 ;; This lets us have a for loop with the list of things you loop over
 ;; up at the top. Note: This is for use with for-each, as it doesn't return the list
 ;(define-syntax for
 ;  (syntax-rules (in)
 ;    ((_ x in y body ...)
 ;     (for-each (lambda (x) body ...)
 ;              y))))

 ; 1 2 3 ... n
 (define (range n)
   (abrange 1 (+ n 1)))

 (define (abrange a b)
   (if (= a b)
       null
       (cons a (abrange (+ a 1) b))))

 ; 0 1 2 3 .. (n-1)
 (define (range0 n)
   (abrange 0 n))

 (define (repeat x n)
   (if (= n 0)
       '()
       (cons x (repeat x (- n 1)))))

 (define (uniquify xs)
   (fold-left (lambda (acc x) (if (member? x acc)
                                  acc
                                  (append acc (list x))))
              '()
              xs))

 (define (zip xs ys)
   (if (or (null? xs) (null? ys))
       '()
       (cons (cons (car xs) (car ys))
             (zip (cdr xs) (cdr ys)))))

 (define (insert pre x post)
   (append pre (cons x post)))

 (define (inserts pre xs post)
   (map (lambda (x) (insert pre x post)) xs))


 (define (minimum xs) (fold-right min (car xs) (cdr xs)))
 (define (maximum xs) (fold-right max (car xs) (cdr xs)))

 (define (string-repeat x n)
   (if (= n 0)
       x
       (string-append x (string-repeat x (- n 1)))))

 (define (join sep strs)
   (cond
    [(null? strs) ""]
    [(= (length strs) 1) (car strs)]
    [else (string-append (car strs) sep (join sep (cdr strs)))]))

 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ; Assertions
 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 (define (assert-equal x y)
  (if (not (equal? x y))
      (begin
        (displayn "*** Failure on assertion")
        (displayn "\t" x)
        (displayn "does not equal")
        (displayn "\t" y)
        (flush-output-port (current-output-port))
        (exit 1))))

 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ; Association lists
 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 (define (has-value k alist)
   (any (lambda (xi) (equal? (second xi) k))
        alist))

 ;; Assoc list -- get a value with a default
 (define (value-of k x default)
   (let ((aa (assoc k x)))
     (if aa
         (second aa)
         default)))

 ;; Assoc list -- get a value with a default
 (define (value-of2 k x default)
   (let ((aa (assoc k x)))
     (if aa
         (cdr aa)
         default)))

 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ; Loading data files
 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


 ;; Split a string
 ;; Stolen from http://schemecookbook.org/Cookbook/StringSplit
 (define str-split
   (lambda (whole-str delimiter)
     (define str-len (string-length whole-str))
     (define split
       (lambda (seg-start seg-end)
         (cond
           [(>= seg-end str-len)
            ; Endgame. Return remains, () if none.
            (if (= seg-start seg-end)
                '()
                (list (substring whole-str seg-start
                                 seg-end)))]
           [(char=? delimiter
                    (string-ref whole-str seg-end))
            ; Delimiter found.
            (if (= seg-start seg-end)
                ; Skip empty segments
                (split (+ 1 seg-start) (+ 1 seg-end))
                ; else cons this segment onto those coming.
                (cons (substring whole-str seg-start seg-end)
                      (split seg-end seg-end)))]
           [else (split seg-start (+ 1 seg-end))]))) ; Next char.
     ; END split
     (split 0 0))) ; Start at 0th char.
 ; END str-split


 ;; Starts with a #
 ;; TODO: allow whitespace before!
 (define (is-comment-line? x)
   (or (eq? (string-length x) 0)
       (string=? (substring x 0 1) "#")
       (string=? (substring x 0 1) ";")))


 (define (is-blank-line? s)
   (all (lambda (ci) (or (char=? ci #\space)
                         (char=? ci #\tab)
                         (char=? ci #\return)
                         (char=? ci #\linefeed)))
        (string->list s)))

 ;; Load a file of S-expressions, skipping comment lines
 ;; NOTE: this skips comment lines
 (define (load-file in)
   (let ((l (get-line in)))
     (if (eof-object? l)
         null
         (if (is-comment-line? l)
             (load-file in)
             (cons (string->S-expression l) (load-file in))))))

 ;; a version that can exit early, if we call a function on the S-expression version of each line
 (define (load-file-ee in exitearly)
   (let ((l (get-line in)))
     (if (eof-object? l)
         null
         (if (is-comment-line? l)
             (load-file in)
             (let ((sl (string->S-expression l)))
               (if (exitearly sl)
                   null
                   (cons sl (load-file-ee in exitearly))))))))

 (define (string->S-expression s)
   ;; NOTE: This only takes the FIRST S-expression in a string
   (let* ((p (open-string-input-port s))
          (ret (read p)))
     (close-input-port p)
     ret))


 ;   (with-input-from-string s (lambda () (read))))

 )
