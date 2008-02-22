;;; A dumb fibonacci test that makes cool patterns.

(define (fibonacci x)
  (cond ((= x 0) (display ".") 1)
        ((= x 1) (display "#") 1)
        (else
         (let ((fx-1 (fibonacci (- x 1))))
           (+ fx-1 (fibonacci (- x 2)))))))
(fibonacci 9)
(newline)
