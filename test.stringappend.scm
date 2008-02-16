;;; Test of string-append function.
(define (strap a b) (display (string-append a b)) (newline))
(strap "hello, " "world")
(strap "" "and")
(strap "null cases" "")
(strap "" "")
