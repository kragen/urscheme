;;; A compiler from a subset of R5RS Scheme to x86 assembly, written in itself.
;; Kragen Javier Sitaker, 2008

;; I think this is nearly the smallest subset of R5RS Scheme that it's
;; practical to write a Scheme compiler in, and I've tried to keep
;; this implementation of it as simple as I can stand.


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
;; - booleans
;; - if

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

;; The calling convention for subroutines is troubling me.  Avoiding
;; register allocation means, I think, that they need to store their
;; arguments in memory.  Perhaps a "local context pointer" register
;; (%ebp? That's what it's for...) can point to the current local
;; variable context, and local variable references can be compiled
;; into indices into that context.  To eventually permit tail-calls
;; (as well as to avoid extra unnecessary code) callees should clean
;; up their own stack frames.  How should closures work?  The caller
;; could load a parent context pointer into some other register or
;; memory location, but maybe something better would be to use one of
;; the indirect-threading or direct-threading strategies from Forth.

;;; Basic Lisp Stuff
;; To build up the spartan language implemented by the compiler to a
;; level where you can actually program in it.  Of course these mostly
;; exist in standard Scheme, but I thought it would be easier to write
;; them in Scheme rather than assembly in order to get the compiler to
;; the point where it could bootstrap itself.

(define lst (lambda args args))         ; identical to standard "list"
(define list-length                     ; identical to standard "length"
  (lambda (list) (if (null? list) 0 (+ 1 (list-length (cdr list))))))
(define lookup                          ; identical to standard "assq"
  (lambda (obj alist)
    (if (null? alist) #f
        (if (eq? obj (car (car alist))) (car alist)
            (lookup obj (cdr alist))))))

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
(define comment (lambda (comment) (insn "# " comment)))

;; Emit a two-argument instruction
(define 2arg 
  (lambda (mnemonic) (lambda (src dest) (insn mnemonic " " src ", " dest))))
;; For example:
(define mov (2arg "mov"))   (define test (2arg "test"))
(define cmpl (2arg "cmpl")) (define lea (2arg "lea"))

;; Emit a one-argument instruction
(define 1arg (lambda (mnemonic) (lambda (rand) (insn mnemonic " " rand))))
(define push (1arg "push")) (define pop-asm (1arg "pop"))
(define jmp (1arg "jmp"))   (define jnz (1arg "jnz"))
(define je (1arg "je"))     (define call (1arg "call"))
(define int (1arg "int"))

;; Currently only using a single zero-argument instruction:
(define ret (lambda () (insn "ret")))

;; Registers:
(define eax "%eax")  (define ebx "%ebx")  
(define ecx "%ecx")  (define edx "%edx")

(define const (lambda (x) (lst "$" x)))
(define indirect (lambda (x) (lst "(" x ")")))

(define syscall (lambda () (int (const "0x80"))))

(define offset 
  (lambda (x offset) (lst (number-to-string offset) (indirect x))))

;; Other stuff for basic asm emission.
(define rodata (lambda () (insn ".section .rodata")))
(define text (lambda () (insn ".text")))
(define label (lambda (label) (emit label ":")))

;; define a .globl label
(define global-label
  (lambda (lbl)
    (begin (insn ".globl " lbl) (label lbl))))

;; new-label: Allocate a new label (e.g. for a constant) and return it.
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

(define tos eax)

;; push-const: Emit code to push a constant onto the abstract stack
(define push-const (lambda (val) (push tos) (mov (const val) tos)))
;; pop: Emit code to discard top of stack.
(define pop (lambda () (pop-asm tos)))

;; dup: Emit code to copy top of stack.
(define dup (lambda () (push tos)))


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
    (insn ".int " (number-to-string (string-length contents)))
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
    (mov (const errlabel) tos)
    (jmp "report_error")))

(define ensure-string-code
  (lambda ()
    (label "ensure_string")
    (comment "ensures that %eax is a string")
    (comment "first, ensure that it's a pointer, not something unboxed")
    (test (const "3") tos)              ; test low two bits
    (jnz "notstring")
    (comment "now, test its magic number")
    (cmpl (const string-magic) (indirect tos))
    (jnz "notstring")
    (ret)))
;; Emit code to ensure that %eax is a string
(define ensure-string (lambda () (call "ensure_string")))
;; Emit code to pull the string pointer and count out of a string
;; being pointed to and push them on the abstract stack
(define extract-string
  (lambda ()
    (ensure-string)
    (lea (offset tos 8) ebx)            ; string pointer
    (push ebx)
    (mov (offset tos 4) tos)))          ; string length

;; Emit code which, given a byte count on top of stack and a string
;; pointer underneath it, outputs the string.
(define write_2
  (lambda ()
    (mov tos edx)                       ; byte count in arg 3
    (pop-asm ecx)                       ; byte string in arg 2
    (mov (const "4") eax)               ; __NR_write
    (mov (const "1") ebx)               ; fd 1: stdout
    (syscall)))                         ; return value is in %eax

;; Emit code to output a string.
;; XXX this needs to have a reasonable return value, and it doesn't!
(define target-display (lambda () (extract-string) (write_2)))
;; Emit code to output a newline.
(define target-newline
  (lambda ()
    (push-const "newline_string")
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
    (mov (const "1") ebx)               ; exit code of program
    (mov (const "1") eax)               ; __NR_exit
    (syscall)))                         ; make system call to exit

(define compile-literal-string-2 (lambda (label) (push-const label)))
(define compile-literal-string
  (lambda (contents)
    (compile-literal-string-2 (constant-string contents))))


;;; Booleans
(define enum-tag 2)
(define double (lambda (val) (+ val val)))
(define tagshift (lambda (val) (double (double val))))
(define nil-value (+ enum-tag (tagshift 256)))
(define true-value (+ enum-tag (tagshift 257)))
(define false-value (+ enum-tag (tagshift 258)))
(define jump-if-false
  (lambda (label)
    (cmpl (const (number-to-string false-value)) tos)
    (pop)
    (je label)))

;;; Compilation
(define compile-var-2
  (lambda (lookupval var)
    (if lookupval ((cdr lookupval)) (error var))))
(define compile-var
  (lambda (var env)
    (compile-var-2 (lookup var env) var)))
(define compile-literal-boolean
  (lambda (b) (push-const (number-to-string (if b 
                                                true-value
                                                false-value)))))
;; compile an expression, discarding result, e.g. for toplevel
;; expressions
(define compile-discarding
  (lambda (expr env)
    (begin (compile-expr expr env)
           (pop))))
(define compile-begin
  (lambda (rands env)
    (if (null? rands) (push-const "31") ; XXX do something reasonable
        (if (null? (cdr rands)) (compile-expr (car rands) env)
            (begin (compile-discarding (car rands) env)
                   (compile-begin (cdr rands) env))))))
(define compile-if-2
  (lambda (cond then else lab1 lab2 env)
    (begin
      (compile-expr cond env)
      (jump-if-false lab1)
      (compile-expr then env)
      (jmp lab2)
      (label lab1)
      (compile-expr else env)
      (label lab2))))
(define compile-if
  (lambda (rands env)
    (if (= (list-length rands) 3)
        (compile-if-2 (car rands) (car (cdr rands)) (car (cdr (cdr rands)))
                      (new-label) (new-label) env)
        (error "if arguments length != 3"))))
(define compile-ration
  (lambda (rator rands env)
    (if (eq? rator 'begin) (compile-begin rands env)
        (if (eq? rator 'if) (compile-if rands env)
            (begin (compile-args rands env)
                   (compile-expr rator env))))))
(define compile-pair
  (lambda (expr env) (compile-ration (car expr) (cdr expr) env)))
(define compile-expr
  (lambda (expr env)
    (if (pair? expr) (compile-pair expr env)
        (if (symbol? expr) (compile-var expr env)
            (if (string? expr) (compile-literal-string expr)
                (if (boolean? expr) (compile-literal-boolean expr)
                    (error expr)))))))
(define compile-args
  (lambda (args env)
    (if (null? args) 0
        (begin
          (compile-expr (car args) env)
          (+ 1 (compile-args (cdr args) env))))))


;;; Main Program

(define basic-env 
  (lst (cons 'display target-display)
       (cons 'newline target-newline)))

(define compile-program
  (lambda (body)
    (string-error-routine)
    (report-error)
    (newline-string-code)
    (ensure-string-code)
    (global-label "main")
    (body)
    (mov (const 0) eax)                 ; return code
    (ret)))

(define my-body
  (lambda ()
    (begin
      (compile-discarding '(begin (display (if #t "hello" "goodbye"))
                                  (display ", world")
                                  (newline)
                                  (display "indeed")) basic-env)
      (compile-discarding '(newline) basic-env))))

(compile-program my-body)
