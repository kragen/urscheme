;;; Test for string->symbol.

;; Ur-Scheme is case-sensitive, so this test needs to compare it with
;; either a case-sensitive Scheme like MzScheme or a
;; lowercase-preferred Scheme like Elk.

(define (xp a) (lambda (b) (display (if (eq? a (string->symbol b)) "X" "P"))))
(for-each (xp 'ha) '("Ha" "ha" "ho" "hee" "hee" "ha" "haaa."))
(newline)
((xp "who") (string-append "w" "ho"))
(newline)
