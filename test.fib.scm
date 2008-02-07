;;; A dumb fibonacci test --- the first recursive code ever compiled.
;; At least, by this compiler.

;; This used to display "+" and "*" depending on which case you fell
;; into, which produced an interesting pattern of
;; "+*+*++*+*++*++*+*++*+", which I think can be produced by a simple
;; L-system.  However, the exact pattern depends on the order of
;; argument evaluation, which differs between this compiler and (at
;; least) MzScheme.

(define fibonacci
  (lambda (x) (if (= x 0) (begin (display "+") 1 )
                  (if (= x 1) (begin (display "+") 1)
                      (+ (fibonacci (- x 1))
                         (fibonacci (- x 2)))))))
(fibonacci 7)
(newline)
