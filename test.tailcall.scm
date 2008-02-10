;;; Test to ensure that tail-calls don't use stack.
(define do-n (lambda (n) (if (= n 0) #t (do-n (- n 1)))))
(do-n 100000)                           ; 50 000 fit in 1024k of stack
(define do-n-with-begin 
  (lambda (n) (if (= n 0) #t (begin (do-n-with-begin (- n 1))))))
(do-n-with-begin 100000)
(define do-n-with-else 
  (lambda (n) (if (not (= n 0)) (do-n-with-else (- n 1))
                  #t)))
(do-n-with-else 100000)
(display "Ok")
(newline)
;; Incidentally, SBCL can count down from a hundred million in 
;; * (time (do-n 100000000))
;; ... 6.172386 seconds of user run time
;; So that's 62ns per iteration, or 16 million iterations per second
;; (on my 700MHz laptop).  This Scheme compiler is getting about 6.5
;; million, which is like 110 cycles per call.  So don't get cocky.
