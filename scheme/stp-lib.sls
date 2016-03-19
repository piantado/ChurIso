(library 
 (stp-lib)
 (export displayn displaynerr null first second third fourth fifth sixth seventh member?
         flatten append apply-append unlist-singleton str-split is-comment-line load-file has-value value-of range all any
         string->S-expression drop assert-equal repeat string-repeat mydisplay 
         )
 (import (rnrs) ) ;  (rnrs io (6)) 
 
 ;; #####################################################################################
 ;; #####################################################################################
 ;; Library functions to make scheme a little nicer.
 ;; #####################################################################################
 ;; #####################################################################################
 
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
 
 (define (first x)  (car x))
 (define (second x) (cadr x))
 (define (third x)  (caddr x))
 (define (fourth x) (cadddr x))
 (define (fifth x)  (cadddr (cdr x)))
 (define (sixth x)  (cadddr (cddr x)))
 (define (seventh x)  (cadddr (cdddr x)))
 
 (define member? member)
 
 
 (define (drop n x)
   (if (<= n 0)
       x
       (cdr (drop (- n 1) x))))
 
 (define (flatten x)
   (cond ((null? x) '())
         ((not (pair? x)) (list x))
         (else (append (flatten (car x))
                       (flatten (cdr x))))))
 
 
 ; (define (flip) (= (random 2) 1))
 
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
 
 (define (depth x)
   (if (list? x)
       (+ 1 (apply max (map depth x)))
       0))
 
 (define (take-n n lst)
   (if (= n 0)
       '()
       (cons (car lst) (take-n (- n 1) (cdr lst)))))
 
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
   (if (= n 0)
       null
       (append (range (- n 1)) (list n))))
 
 (define (repeat x n)
   (if (= n 0)
       x
       (cons x (repeat x (- n 1)))))

  (define (string-repeat x n)
   (if (= n 0)
       x
       (string-append x (string-repeat x (- n 1)))))
 
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
 (define (is-comment-line x)
   (or (eq? (string-length x) 0)
       (string=? (substring x 0 1) "#")
       (string=? (substring x 0 1) ";")))
 
 ;; Load a file of S-expressions, skipping comment lines
 ;; NOTE: this skips comment lines
 (define (load-file in)
   (let ((l (get-line in)))
     (if (eof-object? l)
         null
         (if (is-comment-line l)
             (load-file in)
             (cons (string->S-expression l) (load-file in))))))
 
 ;; a version that can exit early, if we call a function on the S-expression version of each line
 (define (load-file-ee in exitearly)
   (let ((l (get-line in)))
     (if (eof-object? l)
         null
         (if (is-comment-line l)
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