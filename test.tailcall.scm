;;; Test to ensure that tail-calls don't use stack.
(define do-n (lambda (n) (if (= n 0) #t (do-n (- n 1)))))
(do-n 100000)                           ; 50 000 fit in 1024k of stack
(display "Ok") 
(newline)
;; Incidentally, SBCL can count down from a hundred million in 
; * (time (do-n 100000000))
; ... 6.172386 seconds of user run time
;; So that's 62ns per iteration, or 16 million iterations per second.
;; This Scheme compiler is getting about 4-5 million, which is like
;; 140 cycles per call.  So don't get cocky.
