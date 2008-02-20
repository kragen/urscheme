;;; Test for symbol->string.

;; Note that a case-smashing uppercase reference Scheme could cause
;; this test to spuriously fail.
(for-each (lambda (var) (display (symbol->string var)) (display " "))
          '(these are the days that try the souls of men))
(newline)
