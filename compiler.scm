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
;;; - very basic arithmetic: two-argument +, -, and = for integers,
;;;   and decimal numeric constants
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

(define writeln
  (lambda (string) (display string) (newline)))

; Emit a MOV instruction
(define mov
  (lambda (src dest)
    (display "        mov ")
    (display src)
    (display ", ")
    (writeln dest)))

; Emit code which, given a byte count on top of stack and a string
; pointer underneath it, outputs the string.
(define write_2
  (lambda ()
    (mov "%eax" "%edx")                 ; byte count in arg 3
    (writeln "        pop %ecx")        ; byte string in arg 2
    (mov "$4" "%eax")                   ; __NR_write
    (mov "$1" "%ebx")                   ; fd 1: stdout
    (writeln "        int $0x80")))     ; return value is in %eax

; XXX writeln -> asm

(define skeleton 
  (lambda ()
    (writeln "         .section .rodata")
    (writeln "hello:  ")
    (writeln "        .ascii \"hello, world\\n\"")
    (writeln "        .text")
    (writeln "        .globl main")
    (writeln "main:")
    (writeln "        .globl main")
    (writeln "        push $hello")
    (mov "$13" "%eax")
    (write_2)
    (mov "$0" "%eax")                   ; return code
    (writeln "        ret")))

(skeleton)
