;;; Tests for integer->char and char->integer.

(define buf (make-string 1))
(define (print-chartable i max)
  (if (= i max) (newline)
      (begin (string-set! buf 0 (integer->char i))
             (display buf)
             (display (if (= i (char->integer (string-ref buf 0))) " " "!"))
             (print-chartable (+ 1 i) max))))
(print-chartable 32 64)
(print-chartable 64 96)
(print-chartable 96 127)
