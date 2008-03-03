;;; Just funsies.  Splitting words on spaces.
;; Can you believe how much code this takes!?

(define (read-line-list)
  (let ((c (read-char)))
    (if (eof-object? c) c
    (if (eqv? c #\newline) '()
        (cons c (read-line-list))))))

(define (words string) (words-of-list (string->list string)))
(define (words-of-list clst) (map list->string (words-list clst)))
(define (words-list clst)
  (let ((x (car clst)) (y (cdr clst)))
    (if (char-whitespace? x) (words-list y) (word (list x) y))))
(define (word wletters left)
  (cond ((null? left) (list (reverse wletters)))
        ((char-whitespace? (car left))
         (cons (reverse wletters) (words-list (cdr left))))
        (else (word (cons (car left) wletters) (cdr left)))))

(define (go)
  (let ((line (read-line-list)))
    (if (not (eof-object? line))
        (begin
          (for-each (lambda (word) (display word) (display ";") (newline))
                    (words-of-list line))
          (go))
        #f)))

(go)