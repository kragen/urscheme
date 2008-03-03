(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))


(define (repeat thunk count)
  (cond ((= count 1) (thunk))
        (else (thunk) (repeat thunk (1- count)))))

(display (number->string (repeat
                          (lambda ()
                            (tak 18 12 6))
                          100)))
(newline)
