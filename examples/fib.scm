;;; Stupid Fibonacci microbenchmark.

(define (fib n) 
  (if (< n 2) 
      1 
      (+ (fib (1- n)) 
         (fib (- n 2)))))
(define (fib2 n) 
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib2 (1- n)) 
                 (fib2 (- n 2))))))
(define (num n) (display (number->string n)))
(define (show fib n)
  (num n) (display ": ") (num (fib n)) (newline))
(show fib2 35)

;; Both of the above "fib" functions are the same speed.

;; On my machine, (fib2 35) takes 4.34 seconds of user CPU time to
;; run.  (fib 35) in SBCL 0.9.16 takes 2.85 seconds.  This means that
;; for this type of thing, Ur-Scheme is only 50% slower than SBCL ---
;; that is, Ur-Scheme runs at two thirds the speed of SBCL.  That's
;; nothing to be ashamed of, even though SBCL is doing generic
;; arithmetic and transparent bignum overflow.

;; SBCL compiles fib to 56 instructions; Ur-Scheme compiles it to 64
;; instructions and 4 .int directives.

;; The analogous C program (compiled with gcc 4.1.2 -O
;; -fomit-frame-pointer) runs in 0.89 user seconds, almost five times
;; faster.  Its "fib" is 19 instructions.

;; The Python programming language is too much slower for a direct
;; comparison, but fib(30) takes 5.2 user seconds on it.  This
;; predicts that fib(35) would take 58 user seconds, 13 times slower.
