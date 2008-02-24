;;; Test for "append".

(define (wnl x) (write x) (newline))

(wnl (append '() '()))
(wnl (append '() '(a)))
(wnl (append '(a) '(b)))
(wnl (append '(a b) '(c d)))
(wnl (append '(a b c) '(d e f))) 
