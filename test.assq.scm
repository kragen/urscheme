(define null? (lambda (x) (eq? x '())))
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define assq                            ; identical to standard "assq"
  (lambda (obj alist)
    (if (null? alist) #f
        (if (eq? obj (caar alist)) (car alist)
            (assq obj (cdr alist))))))
(define msglist '((father "The President ")
                  (is "is ")
                  (eating "smoking ")
                  (apples "a reefer.")))
(define printmsg
  (lambda (msg) (if (null? msg) (newline)
                    (begin (display (cadr (assq (car msg) msglist)))
                           (printmsg (cdr msg))))))
(printmsg '(father is eating apples))
