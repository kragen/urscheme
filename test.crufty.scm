;;; A simple test program for the compiler.

(define msg "this is a message")
((lambda (hi) (begin (display hi) (newline))) "hi there")

(begin (display (if #t "hello" "goodbye"))
       (display ", world")
       (newline)
       (display "indeed"))
(newline)

((lambda (hello goodbye) (begin (display hello) (newline))) "ok" "NOT OK")

(define fibonacci
  (lambda (x) (if (= x 0) (begin (display "+") 1 )
                  (if (= x 1) (begin (display "+") 1)
                      (+ (fibonacci (- x 1))
                         (fibonacci (- x 2)))))))
(fibonacci 7)
(newline)

(display msg)
(newline)
