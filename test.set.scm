;;; Tests of set!
;; There are three ways variables are stored at the moment: globally,
;; on the stack, and in the heap.

(define global "global")
(define (printglobal) (display global) (newline))
(printglobal)
;; We'd like to ensure that the stack effect of set! (all three kinds)
;; is correct.  It should return exactly one value, not zero or two.
;; But we can't care about what its actual value is, because it's
;; probably something different in the reference Scheme
;; implementation.

;; Executing it in what you'd call void context in C doesn't tell us
;; anything; it's unlikely we'll crash the program by popping off too
;; many stack items, and leaving extra stack items there is safe.  So
;; we have to pass it as an argument to something.  But it has to be
;; in a position where we're using the stuff underneath it; at the
;; moment, that means it needs to be the first argument.

(display (cdr (cons (set! global "global variable") "and ")))
(printglobal)
(set! global "global variable gets set!")
(printglobal)

(define (printlocal local)
  (display (cdr (cons (set! local "local variable always the same")
                      "the right stuff: ")))
  (display local)
  (newline))
(printlocal "a")
(printlocal "b")

(define heap-printer
  (let ((val "original heap var"))
    (lambda (cmd arg)
      (case cmd
        ((set!) (display (cdr (cons (set! val arg) "set-"))))
        ((print) (display val) (newline))
        (else (error "bad cmd" cmd))))))

(heap-printer 'print 0)
(heap-printer 'set! "heap var")
(heap-printer 'print 0)
(heap-printer 'set! "heap var changes!")
(heap-printer 'print 0)
