;;; A compiler from a subset of R5RS Scheme to x86 assembly, written in itself.
;; Kragen Javier Sitaker, 2008-01-03, 04, 05, and 06

;; From the Scheme 9 From Empty Space page:
;; Why in earth write another half-baked implementation of Scheme?
;; It is better than getting drunk at a bar.

;; I had been working on this for a couple of days now when I ran across
;http://www.iro.umontreal.ca/%7Eboucherd/mslug/meetings/20041020/minutes-en.html
;; which says:
;;     How to write a simple Scheme to C compiler, in Scheme. In only
;;     90 minutes! And although not supporting the whole Scheme
;;     standard, the compiler supports fully optimized proper tail
;;     calls, continuations, and (of course) full closures.
;; I was feeling pretty inferior until I started watching the video of
;; the talk, in which Marc Feeley, the speaker, begins by explaining:
;;     So, uh, let me clarify the, uh, the title of the presentation!
;;     The 90 minutes is not the time to write the compiler, but to
;;     explain it.

;; I think this is nearly the smallest subset of R5RS Scheme that it's
;; practical to write a Scheme compiler in, and I've tried to keep
;; this implementation of it as simple as I can stand.  I kind of feel
;; that someone more adept would be having more success at keeping it
;; simple, but hey, it's my first compiler.


;;; Implementation planned:
;; - car, cdr, cons
;; - symbol?, null?, eq?, boolean?, pair?, string?, procedure?,
;;   integer?, char?
;; - eq? works for both chars and symbols
;; - if (with three arguments)
;; - lambda (with fixed numbers of arguments or with a single argument
;;   that gets bound to the argument list (lambda <var> <body>) and a
;;   single body expression)
;; - begin
;; - global variables, with set!
;; - local variables, with lexical scope and set!
;; - top-level define of a variable (not a function)
;; - read, for proper lists, symbols, strings, integers, #t and #f,
;;   and '
;; - eof-object?
;; - garbage collection
;; - strings, with string-set!, string-ref, string literals,
;;   string=?, string-length, and make-string with one argument
;; - which unfortunately requires characters; char=?
;; - very basic arithmetic: two-argument +, -, quotient, remainder,
;;   and = for integers, and decimal numeric constants
;; - recursive procedure calls
;; - display, for strings, and newline
;; - error

;; All of this would be a little simpler if strings were just lists
;; of small integers.

;;; Implemented:
;; - car, cdr, cons
;; - display, for strings, and newline
;; - string and numeric constants
;; - begin
;; - if (with three arguments)
;; - booleans
;; - recursive procedure calls
;; - some arithmetic: +, -, and = for integers
;; - lambda, with fixed numbers of arguments, without nesting
;; - local variables
;; - global variables
;; - strings, with string-set!, string-ref, string literals,
;;   string-length, and make-string with one argument; no string=?
;;   yet.  (And it should probably be implemented in Scheme.)
;; - characters with char=?
;; - dynamic allocation (but no GC yet)

;; Next to implement:
;; - null?, and pair?
;; - variadic functions (that will get us past the first two lines of
;;   compiling itself)
;; - quote! that's going to be interesting.

;;; Not implemented:
;; - call/cc, dynamic-wind
;; - macros, quasiquote
;; - most of arithmetic
;; - vectors
;; - most of the language syntax: dotted pairs, ` , ,@
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

;; The strategy taken herein is to use the x86 as a stack machine
;; (within each function, anyway).  %eax contains the top of stack;
;; %esp points at a stack in memory containing the rest of the stack
;; items.  This eliminates any need to allocate registers; for
;; ordinary expressions, we just need to convert the Lisp code to RPN
;; and glue together the instruction sequences that comprise them.

;; We also use the ordinary x86 stack as a call stack.  See the
;; section ";;; Procedure calls" for details.  This would pose
;; problems for call/cc if I were going to implement it, but I'm not,
;; so I don't care.  You might think it would cause problems for
;; closures of indefinite extent, but the "Implementation of Lua 5.0"
;; paper explains a fairly straightforward way of implementing
;; closures, called "upvalues", that still lets us stack-allocate
;; almost all of the time.

;; Pointers are tagged in the low bits in more or less the usual way:
;; - low bits binary 00: an actual pointer, to an object with an
;;   embedded magic number; examine the magic number to see what it
;;   is.
;; - low bits binary 01: a signed integer, stored in the upper 30 bits.
;; - low bits binary 10: one of a small number of unique objects.  The
;;   first 256 are the chars; following these we have the empty list,
;;   #t, #f, and the EOF object, in that order.  This means that eq?
;;   works to compare chars in this implementation, but that isn't
;;   guaranteed by R5RS, so we can't depend on that property inside
;;   the compiler, since we want to be able to run it on other R5RS
;;   Schemes.
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

(define double (lambda (val) (+ val val)))
(define quadruple (lambda (val) (double (double val))))

(define list (lambda args args))        ; identical to standard "list"
(define length                        ; identical to standard "length"
  (lambda (list) (if (null? list) 0 (+ 1 (length (cdr list))))))
(define assq                            ; identical to standard "assq"
  (lambda (obj alist)
    (if (null? alist) #f
        (if (eq? obj (caar alist)) (car alist)
            (assq obj (cdr alist))))))
;; identical to standard caar, cdar, etc.
(define caar (lambda (val) (car (car val))))
(define cdar (lambda (val) (cdr (car val))))
(define cadr (lambda (val) (car (cdr val))))
(define caddr (lambda (val) (cadr (cdr val))))

(define filter-2
  (lambda (fn lst rest) (if (fn (car lst)) (cons (car lst) rest) rest)))
(define filter           ; this must exist in r5rs but I can't find it
  (lambda (fn lst) (if (null? lst) '()
                       (filter-2 fn lst (filter fn (cdr lst))))))

(define not (lambda (x) (if x #f #t)))  ; identical to standard "not"

;; string manipulation (part of Basic Lisp Stuff)
(define string-append-3
  (lambda (length s2 buf idx)
    (if (= idx (string-length buf)) buf
        (begin
          (string-set! buf idx (string-ref s2 (- idx length)))
          (string-append-3 length s2 buf (+ idx 1))))))
(define string-append-2
  (lambda (s1 s2 buf idx)
    (if (= idx (string-length s1)) 
        (string-append-3 (string-length s1) s2 buf idx)
        (begin
          (string-set! buf idx (string-ref s1 idx))
          (string-append-2 s1 s2 buf (+ idx 1))))))
;; XXX we could get rid of this if we weren't using it for creating error msgs
(define string-append          ; identical to standard "string-append"
  (lambda (s1 s2)
    (string-append-2 s1 s2 (make-string (+ (string-length s1) 
                                           (string-length s2)))
                          0)))
(define char->string-2
  (lambda (buf char) (begin (string-set! buf 0 char) buf)))
(define char->string
  (lambda (char)
    (char->string-2 (make-string 1) char)))
(define string-digit
  (lambda (digit) (char->string (string-ref "0123456789" digit))))
(define number->list-2
  (lambda (num tail)
    (if (= num 0) tail
        (number->list-2 (quotient num 10)
                        (cons (string-digit (remainder num 10)) tail)))))
;; Converts a number into a list of one-digit strings, similar to
;; standard number->string.
(define number->list
  (lambda (num) (if (= num 0) "0" (number->list-2 num '()))))

;; Boy, it sure causes a lot of hassle that Scheme has different types
;; for strings and chars.

(define string-idx-2
  (lambda (string char idx)
    (if (= idx (string-length string)) #f
        (if (char=? (string-ref string idx) char) idx
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
            (if (string? stuff) (display stuff)
                (error (list "emitting" stuff)))))))

;; Emit an indented instruction
(define insn (lambda insn (emit (cons "        " insn))))
(define comment (lambda (comment) (insn "# " comment)))

;; Emit a two-argument instruction
(define twoarg 
  (lambda (mnemonic) (lambda (src dest) (insn mnemonic " " src ", " dest))))
;; For example:
(define mov (twoarg "movl"))  (define movb (twoarg "movb"))
(define movsbl (twoarg "movsbl"))
(define test (twoarg "test")) (define cmp (twoarg "cmpl"))
(define lea (twoarg "lea"))
(define add (twoarg "add"))   (define sub (twoarg "sub"))
(define xchg (twoarg "xchg"))
(define asm-and (twoarg "and"))

;; Emit a one-argument instruction
(define onearg (lambda (mnemonic) (lambda (rand) (insn mnemonic " " rand))))
(define asm-push (onearg "push")) (define asm-pop (onearg "pop"))
(define jmp (onearg "jmp"))       (define jnz (onearg "jnz"))
(define je (onearg "je"))         (define jz je)
(define jnb (onearg "jnb"))
(define call (onearg "call"))     (define int (onearg "int"))
(define inc (onearg "inc"))       (define dec (onearg "dec"))
(define idiv (onearg "idiv"))
;; These have two-arg forms too, but I'm not using them.
(define sal (onearg "sal"))       (define sar (onearg "sar"))

;; Currently only using two zero-argument instructions:
(define ret (lambda () (insn "ret")))
(define repstosb (lambda () (insn "rep stosb")))

;; Registers:
(define eax "%eax")  (define ebx "%ebx")  
(define ecx "%ecx")  (define edx "%edx")
(define ebp "%ebp")  (define esp "%esp")
(define edi "%edi")
(define al "%al")

;; x86 addressing modes:
(define const (lambda (x) (list "$" x)))
(define indirect (lambda (x) (list "(" x ")")))
(define offset 
  (lambda (x offset) (list (number->list offset) (indirect x))))
(define absolute (lambda (x) (list "*" x)))
;; Use this one inside of "indirect" or "offset".
(define index-register
  (lambda (base index size) (list base "," index "," (number->list size))))

(define syscall (lambda () (int (const "0x80"))))


;; Other stuff for basic asm emission.
(define section (lambda (name) (insn ".section " name)))
(define rodata (lambda () (section ".rodata")))
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
      (list "k_" (number->list constcounter)))))

;; stuff to output a Lisp string safely for assembly language
(define dangerous '("\\" "\n" "\""))
(define escapes '("\\\\" "\\n" "\\\""))
(define backslashify-char
  (lambda (char dangerous escapes)
    (if (null? dangerous) (char->string char)
        (if (char=? char (string-ref (car dangerous) 0)) (car escapes)
            (backslashify-char char (cdr dangerous) (cdr escapes))))))
(define backslashify
  (lambda (string idx)
    (if (= idx (string-length string)) '("\"")
        (cons (backslashify-char (string-ref string idx) dangerous escapes)
              (backslashify string (+ idx 1))))))
;; Represent a string appropriately for the output assembly language file.
(define asm-represent-string 
  (lambda (string) (cons "\"" (backslashify string 0))))

(define ascii (lambda (string) (insn ".ascii " (asm-represent-string string))))

;; emit a prologue for a datum to be assembled into .rodata
(define rodatum
  (lambda (labelname)
    (rodata)
    (insn ".align 4")       ; align pointers so they end in binary 00!
    (label labelname)))

(define compile-word (lambda (contents) (insn ".int " contents)))

;;; Stack Machine Primitives
;; As explained earlier, there's an "abstract stack" that includes
;; %eax as well as the x86 stack.

(define tos eax)                        ; top-of-stack register
(define nos (indirect esp))   ; "next on stack", what's underneath TOS

;; push-const: Emit code to push a constant onto the abstract stack
(define push-const (lambda (val) (asm-push tos) (mov (const val) tos)))
;; pop: Emit code to discard top of stack.
(define pop (lambda () (asm-pop tos)))

;; dup: Emit code to copy top of stack.
(define dup (lambda () (asm-push tos)))

;; swap: Emit code to exchange top of stack with what's under it.
(define swap (lambda () (xchg tos nos)))

;;; Some convenience stuff for the structure of the program.

(define stuff-to-put-in-the-header (lambda () #f))
(define concatenate-thunks (lambda (a b) (lambda () (begin (a) (b)))))
(define add-to-header 
  (lambda (proc) (set! stuff-to-put-in-the-header 
                       (concatenate-thunks stuff-to-put-in-the-header proc))))

;; Add code to the header to define an error message.
(define define-error-routine
  (lambda (labelname message)
    (add-to-header (lambda ()
      ((lambda (errlabel)
        (begin
          (label labelname)
          (mov (const errlabel) tos)
          (jmp "report_error")))
       (constant-string (string-append "error: " 
                                       (string-append message "\n"))))))))


;;; Procedure calls.
;; Procedure values are 8 bytes:
;; - 4 bytes: procedure magic number 0xca11ab1e
;; - 4 bytes: pointer to procedure machine code
;; At some point I'll have to add a context pointer.
;; 
;; The number of arguments is passed in %edx; on the machine stack is
;; the return address, with the arguments underneath it; the address
;; of the procedure value that was being called is in %eax.  Callee
;; saves %ebp and pops their own arguments off the stack.  The
;; prologue points %ebp at the arguments.  Return value goes in %eax.
(define procedure-magic "0xca11ab1e")
(add-to-header (lambda ()
    (begin
      (label "ensure_procedure")
      (comment "make sure procedure value is not unboxed")
      (test (const "3") tos)
      (jnz "not_procedure")
      (comment "now test its magic number")
      (cmp (const procedure-magic) (indirect tos))
      (jnz "not_procedure")
      (ret))))
(define ensure-procedure (lambda () (call "ensure_procedure")))
(define compile-apply
  (lambda (nargs)
    (begin
      (ensure-procedure)
      (mov (offset tos 4) ebx)          ; address of actual procedure
      (mov (const (number->list nargs)) edx)
      (call (absolute ebx)))))
(define compile-procedure-prologue
  (lambda (nargs)
    (begin
      (cmp (const (number->list nargs)) edx)
      (jnz "argument_count_wrong")
      (comment "compute desired %esp on return in %ebx and push it")
      (lea (offset (index-register esp edx 4) 4) ebx)
      (asm-push ebx)                    ; push restored %esp on stack
      ;; At this point, if we were a closure, we would be doing
      ;; something clever with the procedure value pointer in %eax.
      (mov ebp tos)                     ; save old %ebp --- in %eax!
      (lea (offset esp 8) ebp))))       ; 8 bytes to skip saved %ebx and %eip
(define compile-procedure-epilogue
  (lambda ()
    (begin
      (asm-pop ebp) ; return val in %eax has pushed saved %ebp onto stack
      (asm-pop ebx)                     ; value to restore %esp to
      (asm-pop edx)                     ; saved return address
      (mov ebx esp)
      (jmp (absolute edx)))))           ; return via indirect jump

(define-error-routine "not_procedure" "not a procedure")
(define-error-routine "argument_count_wrong" "wrong number of arguments")

(define built-in-procedure-2
  (lambda (labelname nargs body bodylabel)
    (begin
      (rodatum labelname)
      (compile-word procedure-magic)
      (compile-word bodylabel)
      (text)
      (label bodylabel)
      (compile-procedure-prologue nargs)
      (body)
      (compile-procedure-epilogue)))) ; maybe we should just centralize
                                      ; that and jump to it? :)
;; Define a built-in procedure so we can refer to it by label and
;; push-const that label, then expect to be able to compile-apply to
;; it later.
(define built-in-procedure-labeled
  (lambda (labelname nargs body)
    (built-in-procedure-2 labelname nargs body (new-label))))
(define global-procedure-2
  (lambda (symbolname nargs body procedure-value-label)
    (begin
      (define-global-variable symbolname procedure-value-label)
      (built-in-procedure-labeled procedure-value-label nargs body))))
;; Add code to define a global procedure known by a certain global
;; variable name to the header
(define define-global-procedure
  (lambda (symbolname nargs body)
    (add-to-header (lambda () 
                     (global-procedure-2 symbolname nargs body (new-label))))))

;; Emit code to fetch the Nth argument of the innermost procedure.
(define get-procedure-arg
  (lambda (n) (begin (asm-push tos)
                     (mov (offset ebp (quadruple n)) tos))))


;;; Memory management.

(add-to-header
 (lambda () 
   (begin (insn ".bss")
          (label "the_arena")
          (insn ".space 1048576")
          (compile-global-variable "arena_pointer" "the_arena"))))

;; Emit code to bump a pointer in a register up, if necessary, to be
;; divisible by 4.
(define align4
  (lambda (reg)
    (begin (add (const "3") reg)
           (asm-and (const "~3") reg))))

(define emit-malloc
  (lambda ()
    (begin (comment "code to allocate memory; tagged number of bytes in %eax")
           (ensure-integer)
           (scheme-to-native-integer eax)
           (align4 eax)
           (mov (indirect "arena_pointer") ebx)
           (add ebx eax)
           (mov eax (indirect "arena_pointer"))
           (mov ebx eax)
           (comment "now %eax points to newly allocated memory"))))

;; XXX still need to implement deallocation and a GC


;;; Strings (on the target)
;; A string consists of the following, contiguous in memory:
;; - 4 bytes of a string magic number 0xbabb1e
;; - 4 bytes of string length "N";
;; - N bytes of string data.
(define string-magic "0xbabb1e")

(define constant-string-2
  (lambda (contents labelname)
    (rodatum labelname)
    (compile-word string-magic)
    (compile-word (number->list (string-length contents)))
    (ascii contents)
    (text)
    labelname))
;; constant-string: Emit code to represent a constant string.
(define constant-string
  (lambda (contents)
    (constant-string-2 contents (new-label))))

(define-error-routine "notstring" "not a string")

(add-to-header (lambda ()
    (label "ensure_string")
    (comment "ensures that %eax is a string")
    (comment "first, ensure that it's a pointer, not something unboxed")
    (test (const "3") tos)              ; test low two bits
    (jnz "notstring")
    (comment "now, test its magic number")
    (cmp (const string-magic) (indirect tos))
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
    (asm-push ebx)
    (mov (offset tos 4) tos)))          ; string length

(define compile-literal-string-2 (lambda (label) (push-const label)))
(define compile-literal-string
  (lambda (contents env)
    (compile-literal-string-2 (constant-string contents))))

(define-global-procedure 'make-string 1
  (lambda () (begin (get-procedure-arg 0)
                    (ensure-integer)
                    (comment "we need 8 bytes more than the string length")
                    (push-const (tagged-integer 8))
                    (emit-integer-addition)
                    (emit-malloc)
                    (mov (const string-magic) (indirect tos))
                    (mov tos ebx)
                    (comment "push address to return, get string length and store it")
                    (get-procedure-arg 0)
                    (scheme-to-native-integer tos)
                    (mov tos (offset ebx 4))
                    (comment "fill string with Xes")
                    (lea (offset ebx 8) edi)
                    (mov tos ecx)
                    (mov (const "'X") eax)
                    (repstosb)
                    (comment "now pop and return the address")
                    (pop))))

(define check-array-bounds
  (lambda ()
    (begin 
      (comment "verify that tagged %eax is in [0, untagged NOS)")
      (ensure-integer)

      ;; Intel manual 253667 explains, "[The SUB instruction]
      ;; evaluates the result for both signed and unsigned integer
      ;; operands and sets the OF and CF flags to indicate an overflow
      ;; in the signed or unsigned result, respectively. The SF flag
      ;; indicates the sign of the signed result."  

      (scheme-to-native-integer eax)
      ;; We can do this with a single unsigned comparison; negative
      ;; array indices will look like very large positive numbers and
      ;; therefore be out of bounds.
      (comment "set flags by (unsigned array index - array max)")
      (cmp nos tos)
      (comment "now we expect unsigned overflow, i.e. borrow/carry.")
      (jnb "index_out_of_bounds")
      (comment "now discard both the index and the bound")
      (pop) (pop))))

(define-error-routine "index_out_of_bounds" "array index out of bounds")

(define-global-procedure 'string-set! 3
  (lambda () 
    (begin 
      (comment "string-set! primitive procedure")
      (get-procedure-arg 0)
      (extract-string)
      (get-procedure-arg 1)
      (check-array-bounds)
      (get-procedure-arg 1)
      (scheme-to-native-integer tos)
      (mov tos edi)
      (comment "now retrieve the address of string bytes from the stack")
      (pop)
      (mov tos ebx)
      (get-procedure-arg 2)
      (ensure-character)
      (scheme-to-native-character tos)
      (movb al (indirect (index-register ebx edi 1)))
      (comment "discard the character and base address")
      (pop) (pop)
      (comment "but we need a return value...")
      (get-procedure-arg 0))))

(define-global-procedure 'string-ref 2
  (lambda ()
    (begin
      (comment "string-ref primitive procedure")
      (get-procedure-arg 0)
      (extract-string)
      (get-procedure-arg 1)
      (check-array-bounds)
      (get-procedure-arg 1)
      (scheme-to-native-character tos)
      (comment "get base address of string data from stack")
      (asm-pop ebx)
      (movb (indirect (index-register tos ebx 1)) al)
      (movsbl al tos)
      (native-to-scheme-character tos))))

(define-global-procedure 'string-length 1
  (lambda ()
    (begin
      (comment "string-length primitive procedure")
      (get-procedure-arg 0)
      (extract-string)
      (asm-pop ebx)
      (native-to-scheme-integer tos))))

;;; conses
;; They're 12 bytes: magic number, car, cdr.  That's all, folks.

(define cons-magic "0x2ce11ed")
(define ensure-cons (lambda () (call "ensure_cons")))
(add-to-header (lambda () (begin (label "ensure_cons")
                                 (test (const "3") tos)
                                 (jnz "not_cons")
                                 (cmp (const cons-magic) (indirect tos))
                                 (jnz "not_cons")
                                 (ret))))
(define-error-routine "not_cons" "not a cons")
(define-global-procedure 'car 1
  (lambda ()
    (begin (get-procedure-arg 0)
           (ensure-cons)
           (mov (offset tos 4) tos))))
(define-global-procedure 'cdr 1
  (lambda ()
    (begin (get-procedure-arg 0)
           (ensure-cons)
           (mov (offset tos 8) tos))))
(define-global-procedure 'cons 2
  (lambda ()
    (begin (push-const (tagged-integer 12))
           (emit-malloc)
           (mov (const cons-magic) (indirect tos))
           (mov tos ebx)
           (get-procedure-arg 0)
           (mov tos (offset ebx 4))
           (pop)
           (get-procedure-arg 1)
           (mov tos (offset ebx 8))
           (pop))))


;;; Other miscellaneous crap that needs reorganizing

;; Emit code which, given a byte count on top of stack and a string
;; pointer underneath it, outputs the string.
(define write_2
  (lambda ()
    (begin
      (mov tos edx)                     ; byte count in arg 3
      (asm-pop ecx)                     ; byte string in arg 2
      (mov (const "4") eax)             ; __NR_write
      (mov (const "1") ebx)             ; fd 1: stdout
      (syscall))))                      ; return value is in %eax

;; Emit code to output a string.
;; XXX this needs to have a reasonable return value, and it doesn't!
(define target-display (lambda () (extract-string) (write_2)))
;; Emit code to output a newline.
(define target-newline
  (lambda ()
    (begin
      (push-const "newline_string")
      (target-display))))
(add-to-header (lambda () (rodatum "newline_string") (constant-string "\n")))

(define-global-procedure 'display 1
  (lambda () (begin (get-procedure-arg 0)
                    (target-display))))
(define-global-procedure 'newline 0 target-newline)
(define-global-procedure 'eq? 2 
  (lambda () (begin (get-procedure-arg 0)
                    (get-procedure-arg 1)
                    (target-eq?))))

;; Emit the code for the normal error-reporting routine
(add-to-header (lambda ()
    (label "report_error")
    (target-display)                    ; print out whatever is in %eax
    (mov (const "1") ebx)               ; exit code of program
    (mov (const "1") eax)               ; __NR_exit
    (syscall)))                         ; make system call to exit


;;; Integers
(define tagshift (lambda (str) (list (number->list str) "<<2")))
(define integer-tag "1")
(define tagged-integer (lambda (int) (list integer-tag " + " (tagshift int))))
(add-to-header (lambda ()
    (label "ensure_integer")
    (test (const "1") tos)
    (jz "not_an_integer")
    (test (const "2") tos)
    (jnz "not_an_integer")
    (ret)))
(define-error-routine "not_an_integer" "not an integer")

(define ensure-integer (lambda () (call "ensure_integer")))
(define assert-equal (lambda (a b) (if (= a b) #t (error "assert failed"))))
;; Emit code to add NOS to TOS; assumes they're already tag-checked
(define emit-integer-addition
  (lambda ()
    (begin (asm-pop ebx)
           (add ebx tos)
           (dec tos))))                 ; fix up tag

(define integer-add
  (lambda (rands env)
    (begin
      (comment "integer add operands")
      (assert-equal 2 (compile-args rands env))
      (comment "now execute integer add")
      (ensure-integer)
      (swap)
      (ensure-integer)
      (emit-integer-addition))))
(define integer-sub
  (lambda (rands env)
    (begin
      (comment "integer subtract operands")
      (assert-equal 2 (compile-args rands env))
      (comment "now execute integer subtract")
      (ensure-integer)
      (swap)
      (ensure-integer)
      (sub tos nos)
      (pop)
      (inc tos))))                      ; fix up tag

;; Emit code to convert a native integer to a tagged integer.
(define native-to-scheme-integer 
  (lambda (reg) (begin (sal reg) (sal reg) (inc reg))))
;; Emit code to convert a tagged integer to a native integer.    
(define scheme-to-native-integer 
  (lambda (reg) (begin (sar reg) (sar reg))))

;; Emit code to divide procedure arg 0 by procedure arg 1
;; This merely zeroes out the tags rather than shifting them off.  The
;; normal tagged representation of an integer N is N*4+1.
;; Unfortunately (N*4+1)/(M*4+1) and (N*4+1) % (M*4+1) don't seem to
;; have particularly nice properties, so we divide (N*4) by (M*4)
;; instead.  (N*4) / (M*4) = N/M, and (N*4) % (M*4) = (N%M) * 4.
;; (Barring overflow.)
(define emit-division-code
  (lambda ()
    (get-procedure-arg 1)
    (ensure-integer)
    (comment "fetch dividend second; idiv wants it in %eax")
    (get-procedure-arg 0)
    (ensure-integer)
    (comment "zero out the tag")
    (dec tos)
    (asm-pop ebx)
    (dec ebx)
    (comment "zero the top half of the dividend")
    (sub edx edx)
    (idiv ebx)))

(define-global-procedure 'remainder 2
  (lambda () (begin (emit-division-code)
                    (comment "remainder (<<2) is in %edx")
                    (mov edx tos)
                    (comment "put the tag back")
                    (inc tos))))
(define-global-procedure 'quotient 2
  (lambda () (begin (emit-division-code)
                    (native-to-scheme-integer tos))))

;;; Booleans and other misc. types
(define enum-tag "2")
(define enum-value (lambda (offset) (list enum-tag " + " (tagshift offset))))
(define nil-value (enum-value 256))
(define true-value (enum-value 257))
(define false-value (enum-value 258))
(define eof-value (enum-value 259))
(define jump-if-false
  (lambda (label)
    (cmp (const false-value) tos)
    (pop)
    (je label)))

;; Emit code to generate an error if TOS isn't a character.
(define ensure-character
  (lambda () (begin (test (const "1") tos)
                    (jnz "not_a_character")
                    (test (const "2") tos)
                    (jz "not_a_character")
                    ;; Intel manual 253666 says, "The comparison is
                    ;; performed by subtracting the second operand
                    ;; from the first operand and then setting the
                    ;; status flags in the same manner as the SUB
                    ;; instruction."  Here we're using AT&T syntax, so
                    ;; that means "the first operand from the second
                    ;; operand", so we expect to set the carry flag
                    ;; here.
                    (cmp (const (enum-value 256)) tos)
                    (jnb "not_a_character"))))

(define-error-routine "not_a_character" "not a character")

;; Emit code to leave an unsigned native character in the register,
;; converting from a tagged character.
(define scheme-to-native-character scheme-to-native-integer)
;; Emit code to convert from an unsigned native character to a tagged
;; character.
(define native-to-scheme-character
  (lambda (reg) (begin (sal reg) (inc reg) (sal reg))))

;; Emit code to push a boolean in place of the top two stack items.
;; It will be #t if they are equal, #f if they are not.
(define target-eq?
  (lambda ()
    ((lambda (label1 label2)
      (asm-pop ebx)
      (cmp ebx tos)
      (je label1)
      (mov (const false-value) tos)
      (jmp label2)
      (label label1)
      (mov (const true-value) tos)
      (label label2)) (new-label) (new-label))))


;;; Global variable handling.

(define global-variable-labels '())
(define global-variables-defined '())

(define add-new-global-variable-binding!
  (lambda (name label)
    (begin (set! global-variable-labels 
                 (cons (cons name label) global-variable-labels))
           label)))
(define allocate-new-global-variable-label!
  (lambda (name) (add-new-global-variable-binding! name (new-label))))
(define global-variable-label-2
  (lambda (name binding)
    (if binding (cdr binding) (allocate-new-global-variable-label! name))))
;; Return a label representing this global variable, allocating a new
;; one if necessary.
(define global-variable-label
  (lambda (name) 
    (global-variable-label-2 name (assq name global-variable-labels))))

;; Emit code to create a mutable labeled cell, for example for use as
;; a global variable, with a specific assembly label.
(define compile-global-variable
  (lambda (varlabel initial)
    (begin
      (section ".data")
      (label varlabel)
      (compile-word initial)
      (text))))

;; Emit code to create a mutable labeled cell for use as a global
;; variable, bound to a specific identifier.
(define define-global-variable
  (lambda (name initial)
    (if (assq name global-variables-defined) (error "double define" name)
        (begin (compile-global-variable (global-variable-label name) initial)
               (set! global-variables-defined 
                     (cons (list name) global-variables-defined))))))

;; Emit code to fetch from a named global variable.
(define fetch-global-variable
  (lambda (varname)
      (asm-push tos) (mov (indirect varname) tos)))

;; Return a list of undefined global variables.
(define undefined-global-variables
  (lambda ()
    (filter (lambda (pair) (not (assq (car pair) global-variables-defined)))
            global-variable-labels)))

;; This runs at the end of compilation to report any undefined
;; globals.  The assumption is that you're recompiling frequently
;; enough that there will normally only be one...
(define assert-no-undefined-global-variables
  (lambda ()
    (if (not (null? (undefined-global-variables)))
        (error "error: undefined global" (caar (undefined-global-variables)))
        #t)))

;;; Compilation of particular kinds of expressions
(define compile-var-2
  (lambda (lookupval var)
    (if lookupval ((cdr lookupval)) 
        (fetch-global-variable (global-variable-label var)))))
(define compile-var
  (lambda (var env)
    (compile-var-2 (assq var env) var)))
(define compile-literal-boolean
  (lambda (b env) (push-const (if b true-value false-value))))
(define compile-literal-integer
  (lambda (int env) (push-const (tagged-integer int))))
;; compile an expression, discarding result, e.g. for toplevel
;; expressions
(define compile-discarding
  (lambda (expr env)
    (begin (compile-expr expr env)
           (pop))))
;; Construct an environment binding the local variables of the lambda
;; to bits of code to fetch them.  Handles nesting very incorrectly.
(define lambda-environment
  (lambda (env vars idx)
    (if (null? vars) env
        (cons (cons (car vars) (lambda () (get-procedure-arg idx)))
              (lambda-environment env (cdr vars) (+ idx 1))))))
(define compile-lambda-2
  (lambda (vars body env proclabel jumplabel)
    (begin (comment "jump past the body of the lambda")
           (jmp jumplabel)
           (built-in-procedure-labeled proclabel (length vars) 
             (lambda () (compile-expr body (lambda-environment env vars 0))))
           (label jumplabel)
           (push-const proclabel))))
(define compile-lambda
  (lambda (rands env) (begin (assert-equal (length rands) 2)
                             (compile-lambda-2 (car rands) (cadr rands) env 
                                               (new-label) (new-label)))))
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
    (if (= (length rands) 3)
        (compile-if-2 (car rands) (cadr rands) (caddr rands)
                      (new-label) (new-label) env)
        (error "if arguments length != 3"))))
(define compile-application
  (lambda (rator env nargs)
    (begin
      (comment "get the procedure")
      (compile-expr rator env)
      (comment "now apply the procedure")
      (compile-apply nargs))))
(define special-syntax-list
  (list (cons 'begin compile-begin)
        (cons 'if compile-if)
        (cons 'lambda compile-lambda)
        (cons '+ integer-add)
        (cons '- integer-sub)))
(define compile-combination-2
  (lambda (rator rands env handler)
    (if handler ((cdr handler) rands env)
        (compile-application rator env (compile-args rands env)))))
(define compile-combination
  (lambda (rator rands env)
    (compile-combination-2 rator rands env (assq rator special-syntax-list))))
(define compile-pair
  (lambda (expr env) (compile-combination (car expr) (cdr expr) env)))
(define compilation-expr-list
  (list (cons pair? compile-pair)
        (cons symbol? compile-var)
        (cons string? compile-literal-string)
        (cons boolean? compile-literal-boolean)
        (cons integer? compile-literal-integer)))
(define compile-expr-2
  (lambda (expr env handlers)
    (if (null? handlers) (error expr)
        (if ((caar handlers) expr) ((cdar handlers) expr env)
            (compile-expr-2 expr env (cdr handlers))))))
(define compile-expr
  (lambda (expr env) (compile-expr-2 expr env compilation-expr-list)))
(define compile-args-2
  (lambda (args env n) (begin (compile-expr (car args) env)
                              (+ 1 n))))
(define compile-args
  (lambda (args env)
    (if (null? args) 0
        (compile-args-2 args env (compile-args (cdr args) env)))))

(define compile-toplevel-define
  (lambda (name body env)
    (define-global-variable name nil-value)
    (comment "compute initial value for global variable")
    (compile-expr body env)
    (comment "initialize global variable with value")
    (mov tos (indirect (global-variable-label name)))
    (pop)))

(define global-env '())

(define compile-toplevel
  (lambda (expr)
    (if (eq? (car expr) 'define) 
        (compile-toplevel-define (cadr expr) (caddr expr) global-env)
        (compile-discarding expr global-env))))

;;; Main Program

(define compile-program
  (lambda (body)
    (begin
      (stuff-to-put-in-the-header)

      (global-label "_start")         ; allow compiling with -nostdlib
      (insn ".weak _start")     ; but also allow compiling with stdlib
      (global-label "main")     ; with entry point of main, not _start

      (compile-toplevel-define '= 'eq? global-env)
      ;; because chars are unboxed, char=? is eq?
      (compile-toplevel-define 'char=? 'eq? global-env)
      (body)

      (mov (const "1") eax)             ; __NR_exit
      (mov (const "0") ebx)             ; exit code
      (syscall)
      (assert-no-undefined-global-variables))))

(define read-compile-loop
  (lambda ()
    ((lambda (expr)
       (if (eof-object? expr) #t
           (begin (compile-toplevel expr)
                  (read-compile-loop))))
     (read))))

(compile-program read-compile-loop)
