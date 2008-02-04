;;; A compiler from a subset of R5RS Scheme to x86 assembly, written in itself.
;; Kragen Javier Sitaker, 2008
;; I think this is nearly the smallest subset of R5RS Scheme that
;; it's practical to write a Scheme compiler in.


;;; Implementation planned:
;; - car, cdr, cons
;; - symbol?, null?, eq?, boolean?, pair?, string?, procedure?,
;;   integer?, char?
;; - eq? works for both chars and symbols
;; - if, lambda (with fixed numbers of arguments or with a single
;;   argument that gets bound to the argument list (lambda x y (...))
;;   and a single body expression)
;; - begin
;; - variables, with lexical scope and set!
;; - top-level define of a variable (not a function)
;; - read, for proper lists, symbols, strings, integers, and #t and #f
;; - eof-object?
;; - garbage collection
;; - strings, with string-set!, string-ref, string literals,
;;   string=?, string-length, and make-string with one argument
;; - which unfortunately requires characters
;; - very basic arithmetic: two-argument +, -, quotient, remainder,
;;   and = for integers, and decimal numeric constants
;; - recursive procedure calls
;; - display, for strings, and newline
;; - error

;; All of this would be a little simpler if strings were just lists
;; of small integers.


;;; Implemented:
;; - display, for strings
;; - string constants
;; - newline
;; - begin

;;; Not implemented:
;; - call/cc, dynamic-wind
;; - macros, quasiquote
;; - most of arithmetic
;; - vectors
;; - most of the language syntax: dotted pairs, ' ` , ,@
;; - write
;; - proper tail recursion
;; - cond, case, and, or, do, not
;; - let, let*, letrec
;; - delay, force
;; - internal definitions
;; - most of the library procedures for handling lists, characters
;; - eval, apply
;; - map, for-each
;; - multiple-value returns
;; - scheme-report-environment, null-environment


;;; Design notes:

;; The strategy taken herein is to use the x86 as a stack machine.
;; %eax contains the top of stack; %esp points at a stack in memory
;; containing the rest of the stack items.  This eliminates any need
;; to allocate registers; for ordinary expressions, we just need to
;; convert the Lisp code to RPN and glue together the instruction
;; sequences that comprise them.

;; Pointers are tagged in the low bits in more or less the usual way:
;; - low bits binary 00: an actual pointer, to an object with an
;;   embedded magic number; examine the magic number to see what it
;;   is.
;; - low bits binary 01: a signed integer, stored in the upper 30 bits.
;; - low bits binary 10: one of a small number of unique objects.  The
;;   first 256 are the chars; following these we have the empty list,
;;   #t, and #f, in that order.  This means that eq? works to compare
;;   chars in this implementation, but that isn't guaranteed by R5RS,
;;   so we can't depend on that property inside the compiler, since we
;;   want to be able to run it on other R5RS Schemes.
;; So, type-testing consists of testing the type-tag, then possibly
;; testing the magic number.  In the usual case, we'll jump to an
;; error routine if the type test fails, which will exit the program.
;; I'll add more graceful exits later.


;;; Basic Lisp Stuff
;; To build up the spartan language implemented by the compiler to a
;; level where you can actually program in it.  Of course these mostly
;; exist in standard Scheme, but I thought it would be easier to write
;; them in Scheme rather than assembly in order to get the compiler to
;; the point where it could bootstrap itself.

(define lst (lambda args args))         ; identical to standard "list"

;; string manipulation (part of Basic Lisp Stuff)
(define string-concatenate-3
  (lambda (length s2 buf idx)
    (if (= idx (string-length buf)) buf
        (begin
          (string-set! buf idx (string-ref s2 (- idx length)))
          (string-concatenate-3 length s2 buf (+ idx 1))))))
(define string-concatenate-2
  (lambda (s1 s2 buf idx)
    (if (= idx (string-length s1)) 
        (string-concatenate-3 (string-length s1) s2 buf idx)
        (begin
          (string-set! buf idx (string-ref s1 idx))
          (string-concatenate-2 s1 s2 buf (+ idx 1))))))
(define string-concatenate              ; string-append
  (lambda (s1 s2)
    (string-concatenate-2 s1 s2 (make-string (+ (string-length s1) 
                                                (string-length s2)))
                          0)))
(define string-of-char-2
  (lambda (buf char) (begin (string-set! buf 0 char) buf)))
(define string-of-char
  (lambda (char)
    (string-of-char-2 (make-string 1) char)))
(define string-digit
  (lambda (digit) (string-of-char (string-ref "0123456789" digit))))
;; Note that this strategy is O(N^2) in the number of digits.
(define number-to-string-2
  (lambda (num)
    (if (= num 0) ""
        (string-concatenate (number-to-string-2 (quotient num 10))
                            (string-digit (remainder num 10))))))
(define number-to-string                ; number->string
  (lambda (num) (if (= num 0) "0" (number-to-string-2 num))))

;; Boy, it sure causes a lot of hassle that Scheme has different types
;; for strings and chars.

(define char-eqv?                       ; identical to standard char=?
  (lambda (a b) (string=? (string-of-char a) (string-of-char b))))
(define string-sub-2
  (lambda (buf string start idx)
    (if (= idx (string-length buf)) buf
        (begin (string-set! buf idx (string-ref string (+ start idx)))
               (string-sub-2 buf string start (+ idx 1))))))
(define string-sub                      ; identical to standard substring
  (lambda (string start end)
    (string-sub-2 (make-string (- end start)) string start 0)))
(define string-idx-2
  (lambda (string char idx)
    (if (= idx (string-length string)) #f
        (if (char-eqv? (string-ref string idx) char) idx
            (string-idx-2 string char (+ idx 1))))))
(define string-idx                      ; returns #f or index into string
  (lambda (string char) (string-idx-2 string char 0)))

;;; Basic Assembly Language Emission

;; emit: output a line of assembly by concatenating the strings in an
;; arbitrarily nested list structure
(define emit (lambda stuff (emit-inline stuff) (newline)))
(define emit-inline
  (lambda (stuff)
    (if (null? stuff) #t
        (if (pair? stuff) 
            (begin (emit-inline (car stuff))
                   (emit-inline (cdr stuff)))
            (display stuff)))))

;; Emit an indented instruction
(define insn (lambda insn (emit (cons "        " insn))))

;; Emit a MOV instruction
(define mov (lambda (src dest) (insn "mov " src ", " dest)))

;; Other stuff for basic asm emission.
(define rodata (lambda () (insn ".section .rodata")))
(define text (lambda () (insn ".text")))
(define label (lambda (label) (emit label ":")))

;; define a .globl label
(define global-label
  (lambda (lbl)
    (begin
      (insn ".globl " lbl)
      (label lbl))))

;; new-label: Allocate a new label (for a constant) and return it.
(define constcounter 0)
(define new-label
  (lambda ()
    (begin
      (set! constcounter (+ constcounter 1))
      (lst "k_" (number-to-string constcounter)))))

;; stuff to output a Lisp string safely for assembly language
(define dangerous "\\\n\"")
(define escapes "\\n\"")
(define backslash (string-ref "\\" 0))  ; I don't have reader syntax for characters
(define backslashify-3
  (lambda (string buf idx idxo backslashed)
    (if backslashed (begin (string-set! buf idxo backslash)
                           (string-set! buf (+ idxo 1) (string-ref escapes backslashed))
                           (backslashify-2 string buf (+ idx 1) (+ idxo 2)))
        (begin (string-set! buf idxo (string-ref string idx))
               (backslashify-2 string buf (+ idx 1) (+ idxo 1))))))
(define backslashify-2
  (lambda (string buf idx idxo)
    (if (= idx (string-length string)) (string-sub buf 0 idxo)
        (backslashify-3 string buf idx idxo 
                        (string-idx dangerous (string-ref string idx))))))
(define backslashify
  (lambda (string)
    (backslashify-2 string (make-string (+ (string-length string)
                                           (string-length string))) 0 0)))
;; Represent a string appropriately for the output assembly language file.
(define asm-represent-string
  (lambda (string)
    (string-concatenate "\"" (string-concatenate (backslashify string) "\""))))

(define ascii (lambda (string) (insn ".ascii " (asm-represent-string string))))

;; emit a prologue for a datum to be assembled into .rodata
(define rodatum
  (lambda (labelname)
    (rodata)
    (insn ".align 4")                   ; so pointers end in 00!
    (label labelname)))


;;; Stack Machine Primitives
;; As explained earlier, there's an "abstract stack" that includes
;; %eax as well as the x86 stack.

;; push_const: Emit code to push a constant onto the abstract stack
(define push_const
  (lambda (const)
    (insn "push %eax")
    (mov const "%eax")))
;; pop: Emit code to discard top of stack.
(define pop (lambda () (insn "pop %eax")))

;; dup: Emit code to copy top of stack.
(define dup (lambda () (insn "push %eax")))


;;; Strings (on the target)
;; A string consists of the following, contiguous in memory:
;; - 4 bytes of a string magic number 0xbabb1e
;; - 4 bytes of string length "N";
;; - N bytes of string data.
(define string-magic "0xbabb1e")

(define constant-string-2
  (lambda (contents labelname)
    (rodatum labelname)
    (insn ".int " string-magic)
    (insn ".int "(number-to-string (string-length contents)))
    (ascii contents)
    (text)
    labelname))
;; constant-string: Emit code to represent a constant string.
(define constant-string
  (lambda (contents)
    (constant-string-2 contents (new-label))))

(define string-error-routine
  (lambda ()
    (string-error-routine-2 (constant-string "type error: not a string\n"))))
(define string-error-routine-2
  (lambda (errlabel)
    (label "notstring")
    (mov (lst "$" errlabel) "%eax")
    (insn "jmp report_error")))

(define ensure-string-code
  (lambda ()
    (label "ensure_string")
    (insn "# ensures that %eax is a string")
    (insn "# first, ensure that it's a pointer, not something unboxed")
    (insn "test $3, %eax")              ; test low two bits
    (insn "jnz notstring")
    (insn "# now, test its magic number")
    (insn "cmpl $" string-magic ", (%eax)")
    (insn "jnz notstring")
    (insn "ret")))
;; Emit code to ensure that %eax is a string
(define ensure-string (lambda () (insn "call ensure_string")))
;; Emit code to pull the string pointer and count out of a string
;; being pointed to and push them on the abstract stack
(define extract-string
  (lambda ()
    (ensure-string)
    (insn "lea 8(%eax), %ebx")         ; string pointer
    (insn "push %ebx")
    (mov "4(%eax)" "%eax")))           ; string length

;; Emit code which, given a byte count on top of stack and a string
;; pointer underneath it, outputs the string.
(define write_2
  (lambda ()
    (mov "%eax" "%edx")                 ; byte count in arg 3
    (insn "pop %ecx")                   ; byte string in arg 2
    (mov "$4" "%eax")                   ; __NR_write
    (mov "$1" "%ebx")                   ; fd 1: stdout
    (insn "int $0x80")))                ; return value is in %eax

;; Emit code to output a string.
;; XXX this needs to have a reasonable return value, and it doesn't!
(define target-display (lambda () (extract-string) (write_2)))
;; Emit code to output a newline.
(define target-newline
  (lambda ()
    (push_const "$newline_string")
    (target-display)))
(define newline-string-code
  (lambda ()
    (rodatum "newline_string")
    (constant-string "\n")))

;; Emit the code for the normal error-reporting routine
(define report-error
  (lambda ()
    (label "report_error")
    (target-display)                    ; print out whatever is in %eax
    (mov "$1" "%ebx")                   ; exit code of program
    (mov "$1" "%eax")                   ; __NR_exit
    (insn "int $0x80")))                ; make system call to exit

(define compile-literal-string-2 (lambda (label) (push_const (lst "$" label))))
(define compile-literal-string
  (lambda (contents)
    (compile-literal-string-2 (constant-string contents))))

;;; Compilation
(define compile-var
  (lambda (var)
    (if (eq? var 'display) (target-display)
        (if (eq? var 'newline) (target-newline)
            (error var)))))
;; compile an expression, discarding result, e.g. for toplevel
;; expressions
(define compile-discarding
  (lambda (expr)
    (begin (compile-expr expr)
           (pop))))
(define compile-begin
  (lambda (rands)
    (if (null? rands) (push_const "$31") ; XXX do something reasonable
        (if (null? (cdr rands)) (compile-expr (car rands))
            (begin (compile-discarding (car rands))
                   (compile-begin (cdr rands)))))))
(define compile-ration
  (lambda (rator rands)
    (if (eq? rator 'begin) (compile-begin rands)
        (begin (compile-args rands)
               (compile-expr rator)))))
(define compile-pair
  (lambda (expr) (compile-ration (car expr) (cdr expr))))
(define compile-expr
  (lambda (expr)
    (if (pair? expr) (compile-pair expr)
        (if (symbol? expr) (compile-var expr)
            (if (string? expr) (compile-literal-string expr)
                (error expr))))))
(define compile-args
  (lambda (args)
    (if (null? args) 0
        (begin
          (compile-expr (car args))
          (+ 1 (compile-args (cdr args)))))))

;;; Main Program

(define compile-program
  (lambda (body)
    (string-error-routine)
    (report-error)
    (newline-string-code)
    (ensure-string-code)
    (global-label "main")
    (body)
    (mov "$0" "%eax")                   ; return code
    (insn "ret")))

(define my-body
  (lambda ()
    (begin
      (compile-discarding '(begin (display "hello, world")
                                  (newline)
                                  (display "goodbye, world")))
      (compile-discarding '(newline)))))

(compile-program my-body)
