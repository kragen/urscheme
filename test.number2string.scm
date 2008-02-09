;;; A simple example program --- a decimal print routine
;; Extracted from compiler.scm.

(define char->string-2
  (lambda (buf char) (begin (string-set! buf 0 char) buf)))
(define char->string
  (lambda (char)
    (char->string-2 (make-string 1) char)))
(define string-digit
  (lambda (digit) (char->string (string-ref "0123456789" digit))))
;; Note that this strategy is O(N^2) in the number of digits.
(define number->string-2
  (lambda (num)
    (if (= num 0) ""
        (string-append (number->string-2 (quotient num 10))
                       (string-digit (remainder num 10))))))
(define number->string        ; identical to standard "number->string"
  (lambda (num) (if (= num 0) "0" (number->string-2 num))))
(define printnum
  (lambda (num) (begin (display (number->string num)) (newline))))

(display (string-digit 6)) (newline)
(display (string-digit 3)) (newline)
(display "ok") (newline)
(display (string-append (string-digit 6) (string-digit 9))) (newline)
(printnum 0)
(printnum 1)
(printnum 7)
(printnum 10)
(printnum 12)
(printnum 694269)
