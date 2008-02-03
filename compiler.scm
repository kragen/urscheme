;;; A compiler from a subset of R5RS Scheme to x86 assembly, written in itself.
;;; Kragen Javier Sitaker, 2008
;;; I think this is nearly the smallest subset of R5RS Scheme that
;;; it's practical to write a Scheme compiler in.

;;; Implemented:
;;; - car, cdr, cons
;;; - symbol?, null?, eq?, boolean?, pair?, string?, procedure?,
;;;   integer?, char?
;;; - if, lambda (with fixed numbers of arguments and a single body
;;;   expression)
;;; - begin
;;; - variables, with lexical scope
;;; - top-level define of a variable (not a function)
;;; - read, for proper lists, symbols, strings, integers, and #t and #f
;;; - eof-object?
;;; - garbage collection
;;; - strings, with string-set!, string-ref, string literals,
;;;   string=?, string-length, and make-string with one argument
;;; - which unfortunately requires characters
;;; - very basic arithmetic: two-argument +, -, quotient, remainder,
;;;   and = for integers, and decimal numeric constants
;;; - recursive procedure calls
;;; - display, for strings, and newline

;;; All of this would be a little simpler if strings were just lists
;;; of small integers.

;;; Not implemented:
;;; - call/cc, dynamic-wind
;;; - macros, quasiquote
;;; - most of arithmetic
;;; - vectors
;;; - most of the language syntax: dotted pairs, ' ` , ,@
;;; - write
;;; - proper tail recursion
;;; - set!
;;; - cond, case, and, or, do, not
;;; - let, let*, letrec
;;; - delay, force
;;; - internal definitions
;;; - most of the library procedures for handling lists, characters
;;; - eval, apply
;;; - map, for-each
;;; - multiple-value returns
;;; - scheme-report-environment, null-environment

;;; The strategy taken herein is to use the x86 as a stack machine.
;;; %eax contains the top of stack; %esp points at a stack in memory
;;; containing the rest of the stack items.  This eliminates any need
;;; to allocate registers; for ordinary expressions, we just need to
;;; convert the Lisp code to RPN and glue together the instruction
;;; sequences that comprise them.

;;; Pointers are tagged in the low bits in more or less the usual way:
;;; - low bits binary 00: an actual pointer, to an object with an
;;;   embedded magic number; examine the magic number to see what it
;;;   is.
;;; - low bits binary 01: an integer, stored in the upper 30 bits.
;;; So, type-testing consists of testing the type-tag, then possibly
;;; testing the magic number.  In the usual case, we'll jump to an
;;; error routine if the type test fails, which will exit the program.
;;; I'll add more graceful exits later.

(define writeln
  (lambda (string) (display string) (newline)))

; Emit a line of assembly
(define asm writeln)

; Emit an indent
(define indent (lambda () (display "        ")))
; Emit an indented instruction
(define insn (lambda (insn) (indent) (writeln insn)))

; Emit a MOV instruction
(define mov
  (lambda (src dest)
    (indent)
    (display "mov ")
    (display src)
    (display ", ")
    (writeln dest)))

; Emit code which, given a byte count on top of stack and a string
; pointer underneath it, outputs the string.
(define write_2
  (lambda ()
    (mov "%eax" "%edx")                 ; byte count in arg 3
    (insn "pop %ecx")                   ; byte string in arg 2
    (mov "$4" "%eax")                   ; __NR_write
    (mov "$1" "%ebx")                   ; fd 1: stdout
    (insn "int $0x80")))                ; return value is in %eax

; Emit code to push a constant onto the abstract stack
(define push_const
  (lambda (const)
    (insn "push %eax")
    (mov const "%eax")))
    
; Emit code to discard top of stack.
(define pop (lambda () (insn "pop %eax")))

; Emit code to copy top of stack.
(define dup (lambda () (insn "push %eax")))

;;; Other stuff for basic asm emission.
(define rodata (lambda () (insn ".section .rodata")))
(define text (lambda () (insn ".text")))
(define label (lambda (label) (display label) (asm ":")))
(define ascii
  (lambda (string)
    (indent)
    (display ".ascii \"")
    (display string)
    (display "\"")
    (newline)))
;; define a .globl label
(define global-label
  (lambda (lbl)
    (indent)
    (display ".globl ")
    (writeln lbl)
    (label lbl)))

;;; So, strings.  A string consists of the following, contiguous in memory:
;;; - 4 bytes of a string magic number, 195801581 (0xbabb1ed)
;;; - 4 bytes of string length "N";
;;; - N bytes of string data.
(define string-magic "195801581") ; maybe later I'll add hex constants
(define string-error-routine
  (lambda ()
    (rodata)
    (label "notstringmsg")
    (ascii "type error: not a string")
    (text)
    (label "notstring")
    (mov "$notstringmsg" "%eax")
    (insn "jmp report_error")))
;; Emit code to ensure that %eax is a string
(define ensure-string
  (lambda ()
    ;; ensure that it's not an unboxed int
    (mov "%eax" "%ebx")
    (insn "and $3, %ebx")
    (insn "jnz notstring")
    ;; now fetch from it
    (dup)
    (mov "(%eax)" "%eax")
    (indent)
    (display "xor $")
    (display string-magic)
    (writeln ", %eax")
    (insn "jnz notstring")
    (pop)))
;; Emit code to pull the string pointer and count out of a string
;; being pointed to and push them on the abstract stack
(define extract-string
  (lambda ()
    (ensure-string)
    (insn "lea 8(%eax), %ebx")         ; string pointer
    (insn "push %ebx")
    (mov "4(%eax)" "%eax")))           ; string length
;; Emit code to output a string.
(define target-display
  (lambda ()
    (extract-string)
    (write_2)))

;; Emit code to represent a constant string.
(define constant-string
  (lambda (contents)
    (rodata)
    (label "thestring")                 ; XXX
    (indent)
    (display ".int ")
    (writeln string-magic)
    (indent)
    (display ".int ")
    (writeln (number-to-string (string-length contents)))
    (ascii contents)
    (text)))

;;; String manipulation functions for use in the compiler.
;; Of course these mostly exist in standard Scheme, but I thought it
;; would be easier to write them in Scheme rather than assembly in
;; order to get the compiler to the point where it could bootstrap
;; itself.
(define number-to-string                ; number->string
  (lambda (num) (if (= num 0) "0" (number-to-string-2 num))))
(define number-to-string-2
  (lambda (num)
    (if (= num 0) ""
        (string-concatenate (number-to-string-2 (quotient num 10))
                            (string-digit (remainder num 10))))))
(define string-digit
  (lambda (digit) (string-of-char (string-ref "0123456789" digit))))
(define string-of-char
  (lambda (char)
    (string-of-char-2 (make-string 1) char)))
(define string-of-char-2
  (lambda (buf char) (begin (string-set! buf 0 char) buf)))
(define string-concatenate              ; string-append
  (lambda (s1 s2)
    (string-concatenate-2 s1 s2 (make-string (+ (string-length s1) 
                                                (string-length s2)))
                          0)))
(define string-concatenate-2
  (lambda (s1 s2 buf idx)
    (if (= idx (string-length s1)) 
        (string-concatenate-3 (string-length s1) s2 buf idx)
        (begin
          (string-set! buf idx (string-ref s1 idx))
          (string-concatenate-2 s1 s2 buf (+ idx 1))))))
(define string-concatenate-3
  (lambda (length s2 buf idx)
    (if (= idx (string-length buf)) buf
        (begin
          (string-set! buf idx (string-ref s2 (- idx length)))
          (string-concatenate-3 length s2 buf (+ idx 1))))))

(define report-error
  (lambda ()
    (label "report_error")
    (push_const "$15")
    (write_2)
    (mov "$1" "%ebx")                   ; exit code of program
    (mov "$1" "%eax")                   ; __NR_exit
    (insn "int $0x80")))                ; make system call to exit

(define skeleton 
  (lambda ()
    (string-error-routine)
    (report-error)
    (constant-string "hello, world\\n") ; note: this gets miscounted
					; as 14 chars instead of 13
    (global-label "main")
    (push_const "$thestring")
    (target-display)
    (pop)
    (mov "$0" "%eax")                   ; return code
    (insn "ret")))

(skeleton)
