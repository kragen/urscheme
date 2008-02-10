(define char->string-2
  (lambda (buf char) (string-set! buf 0 char) buf))
(define char->string
  (lambda (char)
    (char->string-2 (make-string 1) char)))
(define string-digit
  (lambda (digit) (char->string (string-ref "0123456789" digit))))
(define number->string-2
  (lambda (num tail)
    (if (= num 0) tail
        (number->string-2 (quotient num 10)
                          (string-append (string-digit (remainder num 10)) 
                                         tail)))))
;; Converts a number into a string of digits.
(define number->string                  ; same as standard
  (lambda (num) (if (= num 0) "0" 
                    (if (< num 0) 
                        (string-append "-" (number->string-2 (- 0 num) ""))
                        (number->string-2 num "")))))

(display '"hello, ")
(display (car '("world")))
(newline)

(define add (lambda (x) (+ (car x) (cadr x))))
(display (number->string (add '(3 4)))) (newline)
