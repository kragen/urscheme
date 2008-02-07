(define double (lambda (val) (+ val val)))
(define quadruple (lambda (val) (double (double val))))
(define show-number 
  (lambda (num) (if (= num 0) #t 
                    (begin (display "I") 
                           (show-number (- num 1))))))
(show-number (quadruple 3))
(newline)
