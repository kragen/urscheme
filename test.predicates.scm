(define predicates (list (cons "pair?" pair?) (cons "null?" null?) 
                         (cons "symbol?" symbol?)))
(define values '(("()" ()) ("(hello)" (hello)) ("hello" hello)))
(define testpreds 
  (lambda (preds value)
    (if (not (null? preds))
        (begin
          (display (caar preds)) (display " ")
          (display (car value)) (display ": ")
          (display (if ((cdar preds) (cadr value)) "yes" "no")) (newline)
          (testpreds (cdr preds) value))
        '())))
(define testall (lambda (value) (testpreds predicates value)))
(for-each testall values)
