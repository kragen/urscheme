(define mystring (make-string 3))
(string-set! mystring 0 (string-ref "Y" 0))
(string-set! mystring 1 (string-ref "e" 0))
(string-set! mystring 2 (string-ref "s" 0))
(display mystring)
(newline)
