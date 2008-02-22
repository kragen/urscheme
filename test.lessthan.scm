;;; Test the less-than operator.

(define (num n) (display (number->string n)))
(define (tester n)
  (lambda (m)
    (cond ((< n m) (num n) (display " < ") (num m) (newline))
          (else    (num n) (display " >= ") (num m) (newline)))))
(for-each (tester 4) '(0 1 2 3 4 5 6 7 8 9 10 -1 -100 1000 10000 -10000))
