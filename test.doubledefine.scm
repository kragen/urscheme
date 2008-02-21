;;; Test the ability to re-define something already defined.
;; R5RS requires that (define foo bar) be the same as (set! foo bar)
;; if foo is already defined.  It would be nice to at least issue a
;; warning in this case...

(define foo "bar")
(display foo)
(define foo "foo")
(display foo)
(newline)

