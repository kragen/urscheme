;;; Test for "map" library function.

(for-each display (map (lambda (x) (string-append "<->" x))
                       '("my" "hovercraft" "is" "full" "of" "eels")))
(newline)
