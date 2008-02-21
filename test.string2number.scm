;;; Test string->number.

(for-each (lambda (str) 
            (display (number->string (string->number str)))
            (display " "))
          '("0" "1" "2" "9" "10" "12" "20" "24" "100" "233548830"
            "+1" "+10" "+1976" "-1" "-10" "-1976"))
(newline)
