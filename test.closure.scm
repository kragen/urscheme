(define makedisplayer
  (lambda (message)
    (lambda (message2) (display message) (display message2) (newline))))
((makedisplayer "first message;") "second message;")

;; this doesn't work yet:
; (define (deeper a)
;   (lambda (b)
;     (lambda (c) (display a) (display b) (display c) (newline))))
; (((deeper "this ") "is ") "deeper")
