;;; Test to check for the existence of the (define (foo args) ...)
;;; syntax.

(define (foo arg) (display arg) (newline))
(foo "hello there")
(define (bar . manyargs) (display (car manyargs)) (newline))
(bar "bye" "invisible")
