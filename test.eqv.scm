;;; Test to demonstrate that eqv? compares characters and numbers
;;; correctly, by making a pretty dotplot.

;;; Incidentally, eq? works fine in RScheme, Guile, MzScheme, Elk,
;;; Bigloo, and SCM (as well as Ur-Scheme), but not in tinyscheme.

(define popeye "i yam what i yam, and that's all that i yam")
(define (sqrow xs ys)
  (if (null? xs) (newline)
      (begin
        (if (eqv? (car xs) (car ys))
            (display "##")
            (display "  "))
        (sqrow (cdr xs) ys))))
(define (square-bottom xs ys)
  (if (null? ys) (newline)
      (begin (sqrow xs ys)
             (square-bottom xs (cdr ys)))))
(define (squarelist lst) (square-bottom lst lst))
(define (square string) (squarelist (string->list string)))

(square popeye)
(squarelist '(0 1 0 2 0 3 0 5 0 1 0 2 0 0 0 4 2 1 3 5))