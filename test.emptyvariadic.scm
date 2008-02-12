;;; There was a bug having to do with empty variadic argument lists.
(define emptylist (lambda () (list)))
(for-each (lambda (element) (display "canthappen")) (emptylist))
(newline)
