(define show-number 
  (lambda (num) (if (= num 0) #t 
                    (begin (display "I") 
                           (show-number (- num 1))))))
(show-number (quotient 10 10)) (newline)
(show-number (quotient 20 10)) (newline)
(show-number (quotient 21 10)) (newline)
(show-number (quotient 23 10)) (newline)
(show-number (quotient 27 10)) (newline)
(show-number (quotient 29 10)) (newline)
(show-number (quotient 30 10)) (newline)
