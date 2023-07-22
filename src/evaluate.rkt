#lang racket

; sample S-expressions
(define sexp "(+ 3 2)")
(define sexp2 "(+ 5 (+ 3 2))")

; parse for everything within outermost parentheses
(regexp-replace #rx"\\((.*)\\)" sexp2 "\\1")  ; "+ 5 (+ 3 2)"



