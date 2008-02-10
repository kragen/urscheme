;;; Test the "implicit begin" behavior normally present in lambda.
;; This behavior is required by R5RS.
(define foo
  (lambda (string) (display string) (newline)))
(foo "hello")
(foo "again")
