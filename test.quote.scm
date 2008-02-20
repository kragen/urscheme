;;; Test that quoting works.
;; Covers lists, numbers, booleans, and strings, but not symbols.

(display '"hello, ")
(display (car '("world")))
(newline)

(define add (lambda (x) (+ (car x) (cadr x))))
(display (number->string (add '(3 4)))) (newline)
(display (if (car '(#t #f)) "hi" "bad")) (newline)