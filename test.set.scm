;;; Tests of set!
;; There are three ways variables are stored at the moment: globally,
;; on the stack, and in the heap.

(define global "global")
(define (printglobal) (display global) (newline))
(printglobal)
(set! global "global variable")
(printglobal)
(set! global "global variable gets set!")
(printglobal)

(define (printlocal local)
  (set! local "local variable always the same")
  (display local)
  (newline))
(printlocal "a")
(printlocal "b")

(define heap-printer
  (let ((val "original heap var"))
    (lambda (cmd arg)
      (case cmd
        ((set!) (set! val arg))
        ((print) (display val) (newline))
        (else (error "bad cmd" cmd))))))

(heap-printer 'print 0)
(heap-printer 'set! "heap var")
(heap-printer 'print 0)
(heap-printer 'set! "heap var changes!")
(heap-printer 'print 0)
