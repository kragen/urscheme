;;; Test to verify parsing and compiling of character literals.

(define message "Lisp is one of the oldest languages.")
(define (subst-loop str str2 idx)
  (if (= idx (string-length str)) #f
      (begin
        (if (char=? (string-ref str idx) #\s)
            (string-set! str2 idx #\f)
            (string-set! str2 idx (string-ref str idx)))
        (subst-loop str str2 (+ 1 idx)))))
(define (subst str)
  (let ((buf (make-string (string-length str))))
    (subst-loop str buf 0)
    buf))
(display (subst message))
(newline)
