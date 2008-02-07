(define show-number 
  (lambda (num) (if (= num 0) #t 
                    (begin (display "I") 
                           (show-number (- num 1))))))
(show-number (remainder 5 3))
(newline)
