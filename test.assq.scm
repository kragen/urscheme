(define msglist '((father "The President ")
                  (is "is ")
                  (eating "smoking ")
                  (apples "a reefer.")))
(define printmsg
  (lambda (msg) (if (null? msg) (newline)
                    (begin (display (cadr (assq (car msg) msglist)))
                           (printmsg (cdr msg))))))
(printmsg '(father is eating apples))
