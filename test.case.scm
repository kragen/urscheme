;;; Tests the "case" macro.

(define (vorc x)
  (display (case x
             ((a e i o u) "V")
             ((32 _) " ")
             (else "C"))))
(for-each vorc '(t h i s _ i s _ a _ t e s t))
(newline)
