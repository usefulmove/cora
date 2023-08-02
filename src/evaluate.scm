#lang racket

; convert string s-expression to code list
; string->sexp :: string -> list
(define (string->sexp string)
    (read (open-input-string string)))
; sample s-expressions
(define sexp (string->sexp "(+ 3 2)"))
(define sexp2 (string->sexp "(+ 5 (+ 3 2))"))
(define sexp3 (string->sexp "(/ (- (sqrt 5) 1) 2)"))


; atom? :: object -> boolean
(define (atom? obj)
    (and (not (null? obj))
         (not (pair? obj))))


; evaluate s-expression
; evaluate :: list -> any
(define (evaluate sexp env)
    (cond ([(null? sexp) sexp]
           [(atom? sexp) TODO]  ; if atom then either known symbol, number, . . . OR ???
           [(pair? sexp) TODO]  ; if pair then go into another cond based on car
    ))
)

