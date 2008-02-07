;;; A simple test program for the compiler.

(define msg "this is a message")
((lambda (hi) (begin (display hi) (newline))) "hi there")
(begin (display (if #t "hello" "goodbye"))
       (display ", world")
       (newline)
       (display "indeed"))
(newline)
((lambda (hello goodbye) (begin (display hello) (newline))) "ok" "NOT OK")
(display msg)
(newline)
