;;; Test number->string.

(define printnum
  (lambda (num) (begin (display (number->string num)) (newline))))

(printnum 0)
(printnum 1)
(printnum 7)
(printnum 10)
(printnum 12)
(printnum 694269)
