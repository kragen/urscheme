;;; Test that + and - are first-class values.
;; At first they weren't!

(define (map2 op a b) ; only necessary because Ur-Scheme map is binary
  (cond ((and (null? a) (null? b)) '())
        ((null? a) (error "mismatched list lengths in map2"))
        (else (cons (op (car a) (car b)) (map2 op (cdr a) (cdr b))))))
(define (pr num) (display (number->string num)) (newline))

(for-each pr (map2 + '(0 1 3 5 70) '(1 2 3 4 5)))
(for-each pr (map2 - '(0 1 3 5 70) '(1 2 3 4 5)))
