;;; Some notes on how to add vectors to the Ur-Scheme compiler.

;; And how much complexity it would add.

;;; Vector implementation.
;; A vector consists of the following:
;; - 4 bytes of the vector magic number.
;; - 4 bytes of vector length "N"
;; - 4N bytes of vector data

(define vector-magic something)
(define-global-procedure 'make-vector 1
  (lambda () (get-procedure-arg 0)
             (ensure-integer)
             (asm-and (const "~3") tos)
             (add (const "8") tos)
             (emit-malloc)
             (mov (const vector-magic) (indirect tos))
             (mov tos ebx)
             (get-procedure-arg 0)
             (mov tos (offset ebx 4))
             (lea (offset ebx 8) edi)
             (mov tos ecx)
             (mov (const nil-value) tos)
             (rep-stosd)
             (pop)))

;; make-vector: +16

; minus the following:
; (define (extract-string)
;   (ensure-string)
;   (lea (offset tos 8) ebx)              ; string pointer
;   (asm-push ebx)
;   (mov (offset tos 4) tos))             ; string length

(define (extract-string) (ensure-string) (extract-array))
(define (extract-array)
  (asm-push (offset tos 8))
  (mov (offset tos 4) tos))
(define (extract-vector) (ensure-vector) (extract-array))

;; extract-vector total: -5 lines +5 lines

(define ensure-vector 
  (call-ensure-magic-routine vector-magic "ensure_vector" "not a vector"))

;; ensure-vector total: +2 lines

(define-magic-check-primitive 'vector? vector-magic)

;; vector? total: +1

;; So then we just need vector-ref and vector-set! primitives, and
;; support in wthunk.

;; string-ref looks like this:
; (define-global-procedure 'string-ref 2
;   (lambda ()
;     (comment "string-ref primitive procedure")
;     (get-procedure-arg 0)
;     (extract-string)                    ; string only
;     (get-procedure-arg 1)
;     (check-array-bounds)
;     (get-procedure-arg 1)
;     (scheme-to-native-integer tos)
;     (comment "get base address of string data from stack")
;     (asm-pop ebx)
;     (movzbl (indirect (index-register tos ebx 1)) tos) ; string only
;     (native-to-scheme-character tos)))  ; string only

;; So you could write:
;; array-ref-with-arg: consumes result of extract-array.  leaves base
;; address of array in specified register and index in tos
(define (array-ref-with-arg argnum basereg)
  (get-procedure-arg argnum)
  (check-array-bounds)
  (get-procedure-arg argnum)
  (scheme-to-native-integer tos)
  (comment "get base address of array data from stack")
  (asm-pop basereg))
(define-global-procedure 'string-ref 2
  (lambda ()
    (comment "string-ref primitive procedure")
    (get-procedure-arg 0)
    (extract-string)
    (array-ref-with-arg 1 ebx)
    (movzbl (indirect (index-register tos ebx 1)) tos)
    (native-to-scheme-character tos)))
(define-global-procedure 'vector-ref 2
  (lambda ()
    (comment "vector-ref primitive procedure")
    (get-procedure-arg 0)
    (extract-vector)
    (array-ref-with-arg 1 ebx)
    (mov (indirect (index-register ebx tos 4)) tos)))

;; vector-ref total: +22, -13 lines

;; string-set! looks like this:
; (define-global-procedure 'string-set! 3
;   (lambda () 
;     (comment "string-set! primitive procedure")
;     (get-procedure-arg 0)
;     (extract-string)
;     (get-procedure-arg 1)
;     (check-array-bounds)
;     (get-procedure-arg 1)
;     (scheme-to-native-integer tos)
;     (mov tos edi)
;     (comment "now retrieve the address of string bytes from the stack")
;     (pop)
;     (mov tos ebx)
;     (get-procedure-arg 2)
;     (ensure-character)
;     (scheme-to-native-character tos)
;     (movb al (indirect (index-register ebx edi 1)))
;     (comment "discard the character and base address")
;     (pop) (pop)
;     (comment "but we need a return value...")
;     (get-procedure-arg 0)))
;; But it could be written with the above array-ref-with-arg:
(define-global-procedure 'string-set! 3
  (lambda () 
    (comment "string-set! primitive procedure")
    (get-procedure-arg 0)
    (extract-string)                    ; string only
    (array-ref-with-arg 1 ebx)
    (comment "save index in index register")
    (mov tos edi)
    (get-procedure-arg 2)
    (ensure-character)                  ; string only
    (scheme-to-native-character tos)    ; string only
    (movb al (indirect (index-register ebx edi 1))) ; string only
    (comment "discard the character and index")
    (pop) (pop)
    (comment "but we need a return value...")
    (get-procedure-arg 0)))
;; which saves another 5 lines or so.  But vector-set! is going to
;; look awfully similar.  So: vector-set!: -5, +16.

;; And then we need
(define-global-procedure 'vector-length 1
  (lambda () (extract-vector)
             (asm-pop ebx)
             (native-to-scheme-integer tos)))

;; wthunk support looks like
 ...
    ((vector? x) (display "#") (wthunk (vector->list x) display))
 ...
(define (vector->list vec) (vector->list-2 vec 0))
(define (vector->list-2 vec idx)
  (if (= idx (vector-length vec)) '()
      (cons (vector-ref vec idx) (vector->list-2 vec (1+ idx)))))

;; Complexity costs:
;; vector-set: -5, +16
;; vector-ref: +22, -13 lines
;; vector?: +1
;; ensure-vector: +2
;; extract-vector: +5 -5
;; make-vector: +16
;; vector-length: +4
;; wthunk support..+5
;; total: +66 -23 lines, or net +43 lines.

;;; Crazy idea
;; How much would the compiler really suffer from storing strings as
;; normal vectors of characters?  It would pretty much have to buffer for
;; writes, but then we could write string-set! and vector-set! as follows:
(define (string-set! string index char)
  (cond ((not (char? char)) (error "trying to put a non-char into a a string"
                                   (list string index char)))
        ((not (string? string)) (error "trying to string-set! on a non-string"
                                       (list string index char)))
        (else (array-set! string index char))))
(define (vector-set! vec index val)
  (if (not (vector? vec)) (error "trying to vector-set! on a non-vector"
                                (list vec index val))
      (array-set! vec index val)))
;; And similarly for string-ref and vector-ref.

;; It would make the executable more obscure (you wouldn't be able to
;; see the strings in it very easily), and quadruple the size of its
;; strings.  Its actual legitimate string size is 7300 characters or
;; so at the moment, excluding really short strings, although
;; eliminating duplicates would remove about 12% of that.  So that
;; would be an extra 20K or so, which I think would be ugly.

;;; Benefits.
;; The main benefit I had in mind was that you could build hash
;; tables.  So how much extra complexity are hash tables?  Probably
;; the thing to do is to compute a hash value for each symbol which is
;; stored in the symbol's data structure in memory, and then provide a
;; nonstandard accessor for it.  Then you'd say something like

(define hash-size 8)
(define (make-dict) 
  (let ((rv (make-vector hash-size))) (init-dict rv 0) rv))
(define (init-dict vec i)
  (if (< i (vector-length vec)) (begin (vector-set! vec i '())
                                       (init-dict vec (1+ i)))))
(define (hash-bucket sym) (remainder (symbol-hash sym) hash-size))
(define (dict-bucket sym dict) (vector-ref dict (hash-bucket sym)))
(define (dict-ref sym dict) (assq sym (dict-bucket sym dict)))
(define (dict-add! rec dict)
  (let ((sym (car rec)))
    (let ((old-bucket (dict-bucket sym dict)))
      (vector-set! dict (hash-bucket sym) (cons rec old-bucket)))))

;; and those 13 lines of code (or something like them) would allow
;; you to make these substitutions where you need a speedup:
;; (make-dict)         for '()
;; (dict-ref ...)      for (assq ...)
;; (dict-add! ... foo) for (set! foo (cons ... foo))
