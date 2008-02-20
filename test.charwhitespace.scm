;;; Test for char-whitespace?
(for-each (lambda (c) (display (if (char-whitespace? c) "Y" "N")) (newline))
          (string->list "\n\t 01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-+=~`{[]}\\|:;\"'<,.>/?"))
