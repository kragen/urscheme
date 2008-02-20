;;; A program to test the read-char procedure.
;; Incidentally you can use it to find out which words in
;; /usr/dict/words map to one another under some simple substitution
;; cipher.  Like "eerily", "llanos", "oodles", and "oozing".

(define (digit x) (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" x))
(define (add obj set) (if (memq obj set) set (cons obj set)))
(define (translate-chars uniq all)
  (let ((c (read-char (current-input-port))))
    (cond ((or (eof-object? c) (eqv? c #\newline))
           (if (not (null? all)) 
               (let ((rall (reverse all)))
                 (display 
                  (list->string 
                   (map (lambda (c) (digit (- (length (memq c uniq))
                                              1)))
                   rall)))
                 (display " ")
                 (display (list->string rall))
                 (newline))
               #f)
           (not (eof-object? c)))
          (else (translate-chars (add c uniq) (cons c all))))))
(define (translate-line) (translate-chars '() '()))
    
(define (translate) (if (translate-line) (translate) #f))
(translate)