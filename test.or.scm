;;; Test "or" macro.

(define (foo a b) (display a) b)
(or (foo "x" #t) (foo "y" #f)) (newline)
(or (foo "z" #f) (foo "x" #f)) (newline)
