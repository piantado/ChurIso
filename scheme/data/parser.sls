; parsing routines for ChurIso files that provides some
; simple syntax transformation to S-expressions that are 
; are interpreted by main.sls
(library 
 (data parser)
 (export load-and-parse get-input)
 (import (rnrs)  (stp-lib)  (vicare) (rnrs eval (6)) (data grammar))
 
 ; This environment is just that of "grammar", which defines
 ; the macro %churiso-parse% that turns ChurIso source code into 
 ; a list of constraints
 (define PARSER-ENVIRONMENT
   (environment '(data grammar) '(rnrs) '(vicare) ))
 
 (define (load-and-parse in)
   (let ((l (get-line in)))
     ;(displayn ">>" l)
     (if (eof-object? l)
         null
         (if (or (is-comment-line? l)
                 (is-blank-line? l))
             (load-and-parse in) ; skip the line
             (cons (eval (string->S-expression (string-append "(%churiso-parse% " l ")")) ; add parens and parse 
                         PARSER-ENVIRONMENT)
                   (load-and-parse in))))))

 (define (get-input file) ; a convenience wrapper
   (load-and-parse (open-input-file file)))

 ) ;; end library




