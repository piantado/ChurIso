; parsing routines for ChurIso files that provides some
; simple syntax transformation to S-expressions that are 
; are interpreted by main.sls
(library 
 (parser)
 (export load-and-parse)
 (import (rnrs)  (stp-lib)  (vicare) (rnrs eval (6)) (grammar))
 
 ; This environment is jsut that of "grammar", which defines
 ; the macro %churiso-parse% that turns ChurIso source code into 
 ; a list of constraints
 (define PARSER-ENVIRONMENT
   (environment '(grammar) ))
 
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
 
 
 ;(define all-input (load-and-parse (open-input-file "tmp.txt")))
 ;(displaynerr all-input)
 
 )




