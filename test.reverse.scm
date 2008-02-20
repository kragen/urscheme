;;; A program to test the reverse function.

(define (revstr str) (list->string (reverse (string->list str))))
(display (revstr "Able was I ere I saw Elba. A man, a plan, a canal: Panama!"))
(newline)
