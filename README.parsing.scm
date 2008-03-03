;;; These are some notes I took on different approaches to parsing.
;; They parse a slightly simpler language than Ur-Scheme currently
;; does.  They're a little embarrassing because of how clueless I was,
;; but whatever.

(define (getter fn get) 
  (lambda (stream) (fn stream (char->string (get stream)))))
(define (reader fn) (getter fn read-char))
(define (peeker fn) (getter fn peek-char))
(define (reader-nonwsp fn) (getter fn char-after-whitespace))
(define whitespace " \n")		; no tab!
(define (char-after-whitespace stream)
  (let ((c (read-char stream)))
    (if (string-idx whitespace c) (char-after-whitespace stream) c)))
(define parse
  (reader-nonwsp
   (lambda (stream c)
     (cond ((string=? c "(") (parse-list-tail stream))
	   ((string=? c "\"") (parse-string stream))
	   ((string=? c ";") (discard-comment stream) (parse stream))
	   (else (parse-symbol c stream))))))
(define parse-list-tail
  (peeker (lambda
;;; try again

(define (parse stream)
  (let ((c (char->string (peek-char-skipping-whitespace stream))))
    (cond ((string=? c "(") (read-char stream) (parse-list stream))
	  ((string=? c "\"") (read-char stream) (parse-string stream))
	  ((string=? c ";") (discard-comment stream) (parse stream))
	  ((string=? c ")") (error "extra right paren"))
	  (else (parse-symbol stream)))))
(define (parse-symbol stream)
    (string->symbol (list->string (parse-symbol-chars stream))))
(define (parse-symbol-chars stream)
  (if (string-idx "(); \n" (peek-char stream)) '()
      (cons (read-char stream) (parse-symbol-chars stream))))
(define (parse-list stream)
  (let ((c (char->string (peek-char-skipping-whitespace stream))))
    (cond ((string=? c ")") (read-char stream) '())
	  ((string=? c ".") (read-char stream) (read-dotted-tail stream))
	  (else (let ((hd (parse stream))) (cons hd (parse-list stream)))))))
(define (peek-char-skipping-whitespace stream)
  (if (string-idx whitespace (peek-char stream))
      (begin (read-char stream) (peek-char-skipping-whitespace stream))
      (peek-char stream)))
(define (read-dotted-tail stream)
  (let ((rv (parse stream)))
    (if (string=? (char->string (peek-char-skipping-whitespace stream))
		  ")")
	(begin (read-char stream) rv)
	(error "funky dotted list"))))
(define (parse-string stream) (list->string (parse-string-chars stream)))
(define (parse-string-chars stream)
  (let ((c (char->string (read-char stream))))
    (cond ((string=? c "\"") '())
	  ((string=? c "\\") (cons (read-char stream) 
				   (parse-string-chars stream)))
	  (else (cons (string-ref c 0) (parse-string-chars stream))))))

(define (discard-comment stream)
  (let ((c (char->string (read-char stream))))
    (if (string=? c "\n") '() (discard-comment stream))))

;;; God, what a fucking mess.  And I still don't have #t, #f, etc.
;;; Suppose we have character constants?  Should only be an extra line
;;; if we don't handle named characters...

(define (parse stream)
  (let ((c (peek-char-skipping-whitespace stream)))
    (case c
      (( #\( ) (read-char stream) (parse-list stream))
      (( #\" ) (read-char stream) (parse-string stream))
      (( #\; ) (discard-comment stream) (parse stream))
      (( #\) ) (error "extra right paren"))
      (else (parse-symbol stream)))))
(define (parse-symbol stream)
  (string->symbol (list->string (parse-symbol-chars stream))))
(define (parse-symbol-chars stream)
  (if (string-idx "(); \n" (peek-char stream)) '()
      (cons (read-char stream) (parse-symbol-chars stream))))
(define (parse-list stream)
  (let ((c (peek-char-skipping-whitespace stream)))
    (case c
      (( #\) ) (read-char stream) '())
      (( #\. ) (read-char stream) (read-dotted-tail stream))
      (else (let ((hd (parse stream))) (cons hd (parse-list stream)))))))
(define (peek-char-skipping-whitespace stream)
  (if (string-idx " 	\n" (peek-char stream))
      (begin (read-char stream) (peek-char-skipping-whitespace stream))
      (peek-char stream)))
(define (read-dotted-tail stream)
  (let ((rv (parse stream)))
    (if (eqv? #\) (peek-char-skipping-whitespace stream))
	(begin (read-char stream) rv)
	(error "funky dotted list"))))
(define (parse-string stream) (list->string (parse-string-chars stream)))
(define (parse-string-chars stream)
  (let ((c (read-char stream)))
    (case c
      (( #\" ) '())
      (( #\\ ) (cons (read-char stream) (parse-string-chars stream)))
      (else (cons c (parse-string-chars stream))))))
(define (discard-comment stream)
  (if (eqv? (read-char stream) #\newline) '() (discard-comment stream)))

;; 37 lines instead of 38.  
;;; Clearly a more drastic approach is needed.

(define (ungettable stream)
  (let ((ungot #f) (last #f))
    (lambda cmd (cond ((not (null? cmd)) (set! ungot last))
		      (ungot (let ((result ungot)) (set! ungot #f) result))
		      (else (set! last (read-char stream)) last)))))
(define (parse stream) (parse-2 (ungettable stream)))
(define (parse-2 s) (case (after-wsp s)
		      (( #\( ) (parse-list s))
		      (( #\" ) (parse-string s))
		      (( #\; ) (discard-comment s) (parse s))
		      (( #\) ) (error "extra right paren"))
		      (else (s 'unget) (parse-symbol s))))
(define (parse-symbol s) (string->symbol (list->string (parse-symbol-chars s))))
(define (parse-symbol-chars s) (let ((c (s))) 
				 (if (string-idx "(); \n" c) 
				     (begin (s 'unget) '())
				     (cons c (parse-symbol-chars s)))))
(define (parse-list s) (case (after-wsp s)
			 (( #\) ) '())
			 (( #\. ) (read-dotted-tail s))
			 (else (s 'unget) (let ((hd (parse s))) 
					    (cons hd (parse-list s))))))
(define (after-wsp s)
  (let ((c (s))) (if (string-idx " 	\n" c) (after-wsp s) c)))
(define (read-dotted-tail s)
  (let ((rv (parse s)))
    (if (eqv? #\) (after-wsp s)) rv (error "funky dotted list"))))
(define (parse-string s) (list->string (parse-string-chars s)))
(define (parse-string-chars s)
  (case (s)
    (( #\" ) '())
    (( #\\ ) (cons (s) (parse-string-chars s)))
    (else (s 'unget) (cons (s) (parse-string-chars s)))))
(define (discard-comment s) (if (eqv? (s) #\newline) '() (discard-comment s)))

;; 34 lines, hardly any smaller, but it seems more readable to me.
;;; How about Darius's approach?

(define (read s) (read-dispatch s (skip-blanks s (read-char s)))
(define (skip-blanks s c)
  (if (string-idx whitespace-chars c) (skip-blanks s (read-char s))
      c))
(define whitespace-chars "\n 	")
(define non-symbol-chars "\"()';")
(define (read-dispatch s c)
  (if (eof-object? c) c
      (case c
	(( #\\ ) (read-char-literal s (read-char s)))
	(( #\" ) (read-string s (read-char s)))
	(( #\( ) (read-list s (read-char s)))
	(( #\' ) (list 'quote (read s)))
	(( #\) ) (error "Unbalanced parentheses"))
	(else (string->symbol (list->string (read-symbol (peek-char s))))))))
(define (read-char-literal s c)
  (if (eof-object? c) (error "EOF in character literal") c))
(define (read-string s c)
  (if (eof-object? c) (error "Unterminated string literal")
      (case c
	(( #\" ) '())
	(( #\\ ) (cons (read-char s) (read-string s (read-char s))))
	(else (cons c (read-string s (read-char s)))))))
(define (read-symbol s c)
  (if (or (string-idx whitespace-chars c)
	  (string-idx non-symbol-chars c)) 
      '()
      (begin (read-char) (cons c (read-symbol s (peek-char s))))))
(define (read-list s)
  (read-list-dispatch s (skip-blanks s (read-char s))))
(define (read-list-dispatch s c)
  (if (eof-object? c) (error "Missing right paren")
      (if (eqv? c #\)) '()
	  (cons (read-dispatch s c) (read-list s)))))

;; Hey, that's 34 lines too.  But it supports character literals and
;; proper end-of-file handling instead of comments and dotted tails.
;;; How about if we use the SICP stream approach?

(define (make-stream-node head tail)
  (lambda (cmd) (case cmd ((h) head) ((t) (tail)))))
(define (nodify filestream)
  (make-stream-node (read-char filestream) (lambda () (nodify filestream))))
;; No, let's render eof-objects as the atom 'eof to allow more "case"s:
(define (nodify filestream)
  (let ((c (read-char filestream)))
    (make-stream-node (if (eof-object? c) 'eof c) 
		      (lambda () (nodify filestream)))))
(define (read s) (read-dispatch (nodify s)))
(define (read-dispatch s)
  (case (s 'h)
    (( eof ) (eof-object))
    (( #\( ) (read-list (s 't)))
    (( #\) ) (error "missing left parenthesis"))
    (( #\" ) (read-string (s 't)))
    (( #\; ) (discard-comment (s 't)))
    (else    (read-symbol s))))

(define (eof-object) (read-char (open-input-string "")))

;; Looks nice so far, but when we get into read-list, we run into a
;; problem
(define (read-list s)
  (case (s 'h)
    (( eof ) (error "missing right parenthesis"))
    (( #\) ) '())
    (else    (cons (read-dispatch s) (read-list ????)))))

;; You could rewrite read-dispatch to pass back the current state of
;; the stream...
;;; So now we run into the need for parser combinators.
;; Parsec has a practical set:
;; oneOf "a string" (character class)
;; noneOf "a string" (inverted character class)
;; skipMany1 (thing-to-discard-one-or-more-times)
;; >> concatenation
;; >>= concatenation with value passing
;; many (thing-to-apply-kleene-closure-to-and-make-a-list) and many1
;; <|> alternation
;; And some others as well.  So what would we like it to look like?

(define (make-list lparen contents rparen) contents)
(define (make-improper-list lparen contents dot tail rparen)
  (mil-2 (reverse contents) tail))
(define (mil2 head tail)
  (if (null? head) tail (mil2 (cdr head) (cons (car head) tail))))
(define (make-string q1 contents q2 ) (list->string contents))
(define read
  (either
   (sequence make-list "(" (many read) ")")
   (sequence make-improper-list "(" (many read) "." read ")")
   (sequence make-string
	     "\"" 
	     (many (either (sequence n2 "\\" any-char) (none-of "\"\\")))
	     "\"")))
(define (n2 a b) b)

;; There are a couple of problems with this as written:
;; - it's reliant on error-prone positional match-ups;
;; - since Ur-Scheme doesn't have "apply", make-list and the like will
;;   actually just get a single argument containing a list;
;; - read is defined in terms of read, which is a problem since we
;;   aren't in Haskell.  I can eta-expand it, but because we don't
;;   have automatic currying like in Haskell, I have to know what the
;;   arguments are in order to do that.
;; - it doesn't deal with comments or whitespace;
;; - it backtracks more than necessary.  This avoids more of the
;;   backtracking:

(define (read arg)
  ((either
    (sequence make-list "(" (many read) list-tail)
    (sequence make-string 
	      "\""
	      (many (either (sequence (lambda (a b) b) "\\" any-char)
			    (none-of "\"\\")))
	      "\"")) arg))
(define list-tail
  (either (sequence (lambda (x) '()) ")") 
	  (sequence (lambda (dot expr paren) expr) "." read ")")))
(define (make-list paren contents tail) (mil2 (reverse contents) tail))

;;; So how would we define either, many, and sequence?
;; Let's say they return either (value remaining-data) or #f, and
;; they're called with just remaining data, and we can ignore for the
;; time being the exact-string matching items above.

(define (sequence . args)
  (let ((combiner (car args)) (elements (cdr args)))
    (lambda (stream) (seqloop combiner elements stream '()))))
(define (seqloop combiner elements stream extras)
  (if (null? elements)
      (list #t (combiner (reverse extras)) stream)
      (let ((result ((car elements) stream)))
	(and result			; if the first item was ok...
	     (seqloop combiner 
		      (cdr elements)
		      (cadr result)
		      (cons (car result) extras))))))

;; How about "many"?  It backtracks a bit.

(define (many parse) (lambda (stream) (manyloop parse stream '())))
(define (manyloop parse stream accumulated)
  (let ((result (parse stream)))
    (if result
	(manyloop parse (cadr result) (cons (car result) accumulated))
	(list (reverse accumulated) stream))))

;; So here's "either".  It backtracks more.
(define (either . args) (eithern args))
(define (eithern args)
  (case (length args)
    ((0) (error "either needs at least one alternative"))
    ((1) (car args))
    (else (either2 (car args) (eithern (cdr args))))))
(define (either2 a b) (lambda (stream) (or (a stream) (b stream))))

;; Then we'd need to define none-of, any-char, and the sugared exact
;; string matches above.  (All of the arguments of the elemental
;; combinators have to go through a desugaring function.)  So at this
;; point, the parser combinator system is already roughly the size of
;; the entire state-machine parsers above, and the resulting "grammar"
;; isn't all that clear anyway.  It's pretty cool that you can do that
;; in so little code I guess.
