; (define caar (lambda (x) (car (car x))))
; (define assq                            ; identical to standard "assq"
;   (lambda (obj alist)
;     (if (null? alist) #f
;         (if (eq? obj (caar alist)) (car alist)
;             (assq obj (cdr alist))))))
; hey we need quote now. at least for symbols.
