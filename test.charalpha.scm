;;; Tests for char-alphabetic?

(define (x a b)
  (if (= a b) (newline)
      (begin (display (if (char-alphabetic? (integer->char a)) "!" "."))
             (x (+ 1 a) b))))
(x 0 32)    (x 32 64)   (x 64 96)   (x 96 128) 
; stick to ASCII!
;(x 128 160) (x 160 192) (x 192 224) (x 224 256)
