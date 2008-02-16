;; Test for "cond", which is a macro.
(define foo
  (lambda (arg)
    (cond ((null? arg) (display "null") (newline))
          ((pair? arg) (display "pair") (newline))
          (else (display "other") (newline)))))
(for-each foo '(() () dork bork (a list) (or two) ()))
