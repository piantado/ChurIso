; A grammar for ChurIso made via the macro %churiso-parse%
; that transforms ChurIso source into a list of constraints and
; variables, limits, etc. for use by main.sls

; The types of constraints here give the equality comparison operation in 
; main.sls that is used by each 

; Input files are of the form
; a := b               # definition             
; a = b                # constraint            
; a != b               # not equal constraint   
; a ~= b               # trace equality         
; a ^= b               # constraint or non-halt 
; a in (b c)           # a=b or a=c            
; limit a 5            # limit                  
; variable a b c       # define as variables    

(library 
 (grammar)
 (export %churiso-parse%)
 (import (rnrs)  (stp-lib)  (vicare) (rnrs eval (6)) (evaluation) )
 
 (define-syntax %churiso-parse%
   (syntax-rules (:= ^= ~= == != in limit variable show unique)
     ((_ x == y) (list 'constrain 'normal-form-equal? (quote x) (quote y)))
     ((_ x != y) (list 'constrain 'normal-form-unequal? (quote x) (quote y)))
     ((_ x ^= y) (list 'constrain 'not-normal-form-equal? (quote x) (quote y)))
     ((_ x ~= y) (list 'constrain 'trace-approx-equal? (quote x) (quote y)))
     ((_ x in y) (list 'constrain 'normal-form-in? (quote x) (quote y)))
     
     ((_ variable x ...) (list 'variable (quote x) ...))
     ((_ unique x ...)   (list 'unique   (quote x) ...))
     ((_ show x )        (list 'show     (quote x) ))
     ((_ limit x y)      (list 'limit    (quote x) (quote y)))
     ((_ x := y)         (list 'define   (quote x) (quote y)))
     ((_ x ...) (begin (displaynerr "ChurIso syntax error on: \"" (quote x) ... "\"")
                       (flush-output-port (current-output-port))
                       (exit 1)))
     ))
 )

