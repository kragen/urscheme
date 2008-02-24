;;; Ur-Scheme: A GPL self-hosting compiler for a subset of R5RS to fast x86 asm
;; Copyright (C) 2008  Kragen Javier Sitaker (2008-01-03 through 22)

;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; From the Scheme 9 From Empty Space page:
;;     Why in earth write another half-baked implementation of Scheme?
;;     It is better than getting drunk at a bar.
;; And anyway, I never metacircular compiler I didn't like. (Neil Van-Dyke)

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
;; ("D" means "done")
;; D car, cdr, cons
;; D null?
;; D booleans
;; D eq?, pair?, null?, symbol?, integer?, boolean?, string?, procedure?, char?
;; D if
;; D lambda (with fixed numbers of arguments or with a single argument
;;   that gets bound to the argument list (lambda <var> <body>)
;; D begin
;; D global variables
;; D lexically-scoped local variables
;; D nested scopes and closures
;; D set! for global and local variables
;; D top-level define of a variable (not a function)
;; - read, for proper and improper lists, symbols, strings, integers,
;;   #t and #f, and '  (written, just not provided to other programs yet)
;; D consequently symbols need to store their strings, and we need
;;   string->symbol; other parts of the compiler use symbol->string
;; D eof-object?
;; - garbage collection
;; D strings, with string-set!, string-ref, string literals,
;;   string-length, and make-string with one argument, and string=?
;; D which unfortunately requires characters; char=? and character
;;   literals
;; D very basic arithmetic: two-argument +, -, quotient, remainder,
;;   and = for integers, and decimal numeric constants
;; D < for integers
;; D recursive procedure calls
;; D display, for strings, and newline
;; D error
;; D several other standard procedures: list, length, assq, caar,
;;   cdar, cadr, caddr, not, string-append, for-each (in a limited
;;   fashion), map (in a limited fashion), memq, memv, eqv?,
;;   string->list
;; D several standard macros: cond (without =>), case, or, let
;;   (without tagged looping)

;; D tail-call optimization

;; All of this would be a little simpler if strings were just lists
;; of small integers.

;; Remaining to implement:
;; - make read accessible to programs compiled with the compiler
;;   somehow
;; - garbage collection
;; - maybe output buffering; compiled with itself, it takes 1.3 user
;;   seconds to compile itself, but another 0.6 system seconds because
;;   it makes almost 50 000 system calls, all but 91 of which are
;;   writes to stdout.  So it would compile itself 50% faster with
;;   output buffering.  But segfaults would be harder to diagnose.
;; - fixing (error ...) to print stuff out nicely.

;; There were a bunch of parts of standard Scheme that I implemented
;; at the top of the compiler, which was a little bit silly --- any
;; program to be compiled by this compiler would either have to forgo
;; using those same facilities, or reimplement them itself.

;; Now I have moved them into a prelude called "standard-library" that
;; gets compiled before the user's program, which considerably expands
;; the subset of Scheme supported without adding any complexity to the
;; overall system.  However, it does inflate output executables a bit.

;;; Not implemented:
;; - call/cc, dynamic-wind
;; - user-definable macros
;; - quasiquote
;; - most of arithmetic
;; - vectors
;; - some of the language syntax ` , ,@
;; - do
;; - let*, letrec
;; - delay, force
;; - internal definitions
;; - most of the library procedures for handling lists, characters
;; - eval, apply
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
;; - low bits binary 11: unused.
;; So, type-testing consists of testing the type-tag, then possibly
;; testing the magic number.  In the usual case, we'll jump to an
;; error routine if the type test fails, which will exit the program.
;; I'll add more graceful exits later.



;;; Basic Lisp Stuff
;; Things that I can't find in R5RS, and so I'm not including in
;; standard-library down below.

(define (double val) (+ val val))
(define (quadruple val) (double (double val)))

;; These functions' names come from Common Lisp or Forth.
(define (1+ x) (+ x 1))                 ; duplicated in stdlib
(define (1- x) (- x 1))                 ; duplicated in stdlib

(define (reduce fn lst init)        
  (if (null? lst) init (fn (car lst) (reduce fn (cdr lst) init))))
(define (filter fn lst)  ; this must exist in r5rs but I can't find it
  (if (null? lst) '()
      (let ((first (car lst)) (rest (filter fn (cdr lst))))
        (if (fn first) (cons first rest) rest))))

(define (char->string char)             ; duplicated in stdlib
  (let ((buf (make-string 1))) (string-set! buf 0 char) buf))

(define (assert x why) (if (not x) (error "surprise! error" why) '()))

;; Boy, it sure causes a lot of hassle that Scheme has different types
;; for strings and chars.

;; copies "len" chars from "src" starting at "srcidx" to "dest"
;; starting at "destidx"
;; XXX needs a !
(define (string-blit! src srcidx len dest destidx) ; duplicated in stdlib
  (if (= len 0) #f 
      (begin (string-set! dest destidx (string-ref src srcidx))
             (string-blit! src (1+ srcidx) (1- len) dest (1+ destidx)))))


;;; Basic Assembly Language Emission

;; emit: output a line of assembly by concatenating the strings in an
;; arbitrarily nested list structure
(define assembly-diversions #f)
(define diverted-assembly '())
(define (asm-display stuff)
  (if assembly-diversions (set! diverted-assembly (cons stuff diverted-assembly))
      (display stuff)))
(define (push-assembly-diversion)
  (assert (not assembly-diversions) "already diverted")
  (set! assembly-diversions #t))
(define (pop-diverted-assembly)
  (let ((result (asm-flatten (reverse diverted-assembly))))
    (set! assembly-diversions #f)
    (set! diverted-assembly '())
    result))
(define (emit . stuff) (asm-display (asm-flatten (cons stuff "\n"))))
(define (asm-flatten stuff)
  (let ((buf (make-string (asm-flatten-size stuff))))
    (asm-flatten-inner buf 0 stuff)
    buf))
(define (asm-flatten-size stuff)
  (cond ((null? stuff) 0) 
        ((pair? stuff) (+ (asm-flatten-size (car stuff))
                          (asm-flatten-size (cdr stuff))))
        ((string? stuff) (string-length stuff))
        (else (error "flatten-size" stuff))))
(define (asm-flatten-inner buf idx stuff)
  (cond ((null? stuff) 
         idx)
        ((pair? stuff)
         (asm-flatten-inner buf 
                            (asm-flatten-inner buf idx (car stuff))
                            (cdr stuff)))
        ((string? stuff)
         (string-blit! stuff 0 (string-length stuff) buf idx)
         (+ idx (string-length stuff)))
        (else 
         (error "flattening" stuff))))
    
;; Memoize a one-argument assembly-generating routine.
(define (memo1-asm proc)
  (let ((results '()))
    (lambda (arg)
      (let ((cached (assq arg results)))
        (if cached (begin (asm-display (cadr cached)) (caddr cached))
            (begin
              (push-assembly-diversion)
              (let ((result (proc arg)))
                (let ((output (pop-diverted-assembly)))
                  (set! results (cons (list arg output result) results))
                  (asm-display output)
                  result))))))))

;; Memoize a zero-argument assembly-generating routine.
(define (memo0-asm proc)
  (lambda ()
    (let ((output #f) (result #f))
      (cond (output (asm-display output) result)
            (else   (push-assembly-diversion) 
                    (let ((nresult (proc)))
                      (set! output (pop-diverted-assembly))
                      (set! result nresult)
                      (asm-display output)
                      result))))))

;; Emit an indented instruction
(define (insn . insn) (emit (cons "        " insn)))
(define (comment . comment) (insn "# " comment))

;; Emit a two-argument instruction
(define (twoarg mnemonic) (lambda (src dest) (insn mnemonic " " src ", " dest)))
;; For example:
(define mov (twoarg "movl"))  (define movb (twoarg "movb"))
(define movzbl (twoarg "movzbl"))
(define test (twoarg "test")) (define cmp (twoarg "cmpl"))
(define lea (twoarg "lea"))
(define add (twoarg "add"))   (define sub (twoarg "sub"))
(define xchg (twoarg "xchg"))
(define asm-and (twoarg "and"))

;; Emit a one-argument instruction
(define (onearg mnemonic) (lambda (rand) (insn mnemonic " " rand)))
(define asm-push (onearg "push")) (define asm-pop (onearg "pop"))
(define jmp (onearg "jmp"))       (define jnz (onearg "jnz"))
(define je (onearg "je"))         (define jz je)
(define jnb (onearg "jnb"))       (define jl (onearg "jl"))
(define js (onearg "js"))  
(define call (onearg "call"))     (define int (onearg "int"))
(define inc (onearg "inc"))       (define dec (onearg "dec"))
(define idiv (onearg "idiv"))
;; These have two-arg forms too, but I'm not using them.
(define sal (onearg "sal"))       (define sar (onearg "sar"))

;; Currently only using three zero-argument instructions:
(define (ret) (insn "ret"))
(define (rep-stosb) (insn "rep stosb"))
(define (repe-cmpsb) (insn "repe cmpsb"))

;; Registers:
(define eax "%eax")  (define ebx "%ebx")  
(define ecx "%ecx")  (define edx "%edx")
(define ebp "%ebp")  (define esp "%esp")
(define esi "%esi")  (define edi "%edi")
(define al "%al")

;; x86 addressing modes:
(define (const x) (list "$" x))
(define (indirect x) (list "(" x ")"))
(define (offset x offset) (list (number->string offset) (indirect x)))
(define (absolute x) (list "*" x))
;; Use this one inside of "indirect" or "offset".
(define (index-register base index size)
  (list base "," index "," (number->string size)))

(define (syscall) (int (const "0x80")))


;; Other stuff for basic asm emission.
(define (section name) (insn ".section " name))
(define (rodata) (section ".rodata"))
(define (text) (insn ".text"))
(define (label label) (emit label ":"))

;; define a .globl label
(define (global-label lbl) (insn ".globl " lbl) (label lbl))

;; new-label: Allocate a new label (e.g. for a constant) and return it.
(define old-label-prefixes '())
(define constcounter 0)
(define label-prefix "k")
;; We set the label prefix (and reset the counter) periodically for
;; two reasons.  First, the assembly code is much more readable when
;; it says movl (_cdr_2), %eax; call ensure_procedure, rather than
;; movl (k_321), %eax; call ensure_procedure.  Second, resetting the
;; counter occasionally means that a compiler change that allocates
;; one more or one less label will have a fairly local effect on the
;; assembly output, rather than changing hundreds or thousands of
;; labels, and all the references to them.  This makes the diff output
;; a lot more readable!

;; However, occasionally we'll get the label-prefix set to the same
;; thing twice, at different times.  This can occur because of
;; quasi-name-collisions in the user program, duplicate defines of the
;; same variable, or just somebody naming a variable "k".  In this
;; case, we simply decline to set the label-prefix.

(define (set-label-prefix new-prefix) 
  (let ((new-label-prefix
         (asm-flatten
          (cons "_"
                (escape (symbol->string new-prefix) 0 
                        '("+"    "-" "="  "?" ">"  "<"  "!"    "*"    "/" 
                          ":"  "@"  "^"     "~"     "$"      "%"   "&")
                        '("Plus" "_" "Eq" "P" "Gt" "Lt" "Bang" "Star" "Slash"
                          "Co" "At" "Caret" "Tilde" "Dollar" "Pct" "And"))))))
    (let ((prefix-symbol (string->symbol new-label-prefix)))
      (if (not (memq prefix-symbol old-label-prefixes))
          (begin
            (set! label-prefix new-label-prefix)
            (set! old-label-prefixes (cons prefix-symbol old-label-prefixes))
            (set! constcounter 0))))))

(define (new-label)
  (set! constcounter (1+ constcounter))
  (list label-prefix "_" (number->string constcounter)))

;; stuff to output a Lisp string safely for assembly language
(define (escape-char char dangerous escapes) ; duplicated in stdlib
  (cond ((null? dangerous) (char->string char))
        ((char=? char (string-ref (car dangerous) 0))
         (car escapes))
        (else (escape-char char (cdr dangerous) (cdr escapes)))))
(define (escape string idx dangerous escapes) ; duplicated in stdlib
  (if (= idx (string-length string)) '()
      (cons (escape-char (string-ref string idx) dangerous escapes)
            (escape string (1+ idx) dangerous escapes))))
;; Escape the three necessary characters.  duplicated in stdlib
(define (backslash string) (escape string 0 '("\\"   "\n"  "\"") 
                                            '("\\\\" "\\n" "\\\"")))
;; Represent a string appropriately for the output assembly language file.
(define (asm-represent-string string) (list "\"" (backslash string) "\""))

(define (ascii string) (insn ".ascii " (asm-represent-string string)))

;; emit a prologue for a datum to be assembled into .rodata
(define (rodatum labelname)
  (rodata)
  (comment "align pointers so they end in binary 00")
  (insn ".align 4")
  (label labelname))

(define (compile-word contents) (insn ".long " contents))

;;; Stack Machine Primitives
;; As explained earlier, there's an "abstract stack" that includes
;; %eax as well as the x86 stack.

(define tos eax)                        ; top-of-stack register
(define nos (indirect esp))   ; "next on stack", what's underneath TOS

;; push-const: Emit code to push a constant onto the abstract stack
(define (push-const val) (asm-push tos) (mov (const val) tos))
;; pop: Emit code to discard top of stack.
(define (pop) (asm-pop tos))

;; dup: Emit code to copy top of stack.
(define (dup) (asm-push tos))

;; swap: Emit code to exchange top of stack with what's under it.
(define (swap) (xchg tos nos))

;;; Some convenience stuff for the structure of the program.

(define stuff-to-put-in-the-header (lambda () #f))
(define (concatenate-thunks a b) (lambda () (a) (b)))
(define (add-to-header proc) 
  (set! stuff-to-put-in-the-header 
        (concatenate-thunks stuff-to-put-in-the-header proc)))

;; Add code to the header to define an error message.
(define (define-error-routine labelname message)
  (add-to-header (lambda ()
    (let ((errlabel         
           (constant-string (string-append "error: " 
                                           (string-append message "\n")))))
      (label labelname)
      (mov (const errlabel) tos)
      (jmp "report_error")))))

;; Emit the code for the normal error-reporting routine
(add-to-header (lambda ()
    (label "report_error")
    (extract-string)
    (comment "fd 2: stderr")
    (mov (const "2") ebx)
    (write_2)
    (mov (const "1") ebx)               ; exit code of program
    (mov (const "1") eax)               ; __NR_exit
    (syscall)))                         ; make system call to exit

(define (compile-tag-check-procedure desired-tag)
  (get-procedure-arg 0)
  (asm-and (const "3") tos)
  (cmp (const desired-tag) tos)
  (je "return_true")
  (jmp "return_false"))


;;; Procedure calls.
;; Procedure values are at least 12 bytes:
;; - 4 bytes: procedure magic number 0xca11ab1e
;; - 4 bytes: pointer to procedure machine code
;; - 4 bytes: number of closed-over variables --- zero for top-level
;;   procedures.  This is not needed by the code inside the closure.
;; Pointers to any closed-over variables follow.
;; 
;; The number of arguments is passed in %edx; on the machine stack is
;; the return address, with the arguments underneath it; the address
;; of the procedure value that was being called is in %eax.  Callee
;; saves %ebp and pops their own arguments off the stack.  The
;; prologue points %ebp at the arguments.  Return value goes in %eax.
(define procedure-magic "0xca11ab1e")
(add-to-header (lambda ()
      (label "ensure_procedure")
      (if-not-right-magic-jump procedure-magic "not_procedure")
      (ret)))
(define (ensure-procedure) (call "ensure_procedure"))
(define compile-apply 
  (memo1-asm (lambda (nargs)
    (ensure-procedure)
    (mov (offset tos 4) ebx)            ; address of actual procedure
    (mov (const (number->string nargs)) edx)
    (call (absolute ebx)))))
(define compile-tail-apply 
  (memo1-asm (lambda (nargs)
    (comment "Tail call; nargs = " (number->string nargs))
    (comment "Note %esp points at the last thing pushed,")
    (comment "not the next thing to push.  So for 1 arg, we want %ebx=%esp")
    (lea (offset esp (quadruple (1- nargs))) ebx)
    (pop-stack-frame edx)
    (copy-args ebx nargs 0)
    (asm-push edx)
    (ensure-procedure)
    (mov (offset tos 4) ebx)
    (mov (const (number->string nargs)) edx)
    (jmp (absolute ebx)))))
(define (copy-args basereg nargs i)
  (if (not (= nargs i))
      (begin (asm-push (offset basereg (- 0 (quadruple i))))
             (copy-args basereg nargs (1+ i)))))

;; package up variadic arguments into a list.  %ebp is fully set up,
;; so we can index off of it to find each argument, and %edx is the
;; total number of arguments.  Only trouble is that we have to push
;; %edx and our loop counter and whatever if we want to save them
;; across a call to cons.
(add-to-header 
 (lambda () 
   (label "package_up_variadic_args")
   (comment "we have %ebp pointing at args, %edx with count")
   (comment "saved %ebp in %eax.  zero-iterations case: return nil")
   (push-const nil-value)
   (label "variadic_loop")
   (dec edx)
   (comment "fucking dec doesn't update carry flag, so jump if negative")
   (js "variadic_loop_end")
   (comment "calling cons clobbers registers, so push %edx")
   (asm-push edx)
   (comment "now push args for cons")
   (asm-push eax)
   (asm-push (offset (index-register ebp edx 4) 4))
   (comment "give cons its argument count")
   (mov (const "2") edx)
   (call "cons")
   (comment "now the args are popped and we have new list in %eax")
   (asm-pop edx)
   (jmp "variadic_loop")
   (label "variadic_loop_end")
   (comment "now we pretend procedure was called with the list as first arg")
   (mov eax (indirect ebp))
   (comment "restore %eax to value on entry to package_up_variadic_args")
   (pop)
   (ret)))
(define (compile-variadic-prologue)
  (comment "make space for variadic argument list")
  (asm-pop ebx)
  (asm-push ebx)
  (asm-push ebx)
  (comment "push desired %esp on return")
  (lea (offset (index-register esp edx 4) 8) ebx)
  (asm-push ebx)

  (asm-push ebp)                        ; save old %ebp
  (lea (offset esp 12) ebp)  ; 12 bytes to skip saved %ebp, %ebx, %eip

  (call "package_up_variadic_args"))

(define compile-procedure-prologue 
  (memo1-asm (lambda (nargs)
    (if (null? nargs) (compile-variadic-prologue)
        (begin
          (comment "compute desired %esp on return in %ebx and push it")
          (lea (offset (index-register esp edx 4) 4) ebx)
          (asm-push ebx)

          (asm-push ebp)                  ; save old %ebp
          (lea (offset esp 12) ebp) ; 12 bytes to skip saved %ebp, %ebx, %eip

          (cmp (const (number->string nargs)) edx)
          (jnz "argument_count_wrong"))))))
(define compile-procedure-epilogue
  (memo0-asm (lambda ()
    (comment "procedure epilogue")
    (comment "get return address")
    (pop-stack-frame edx)
    (jmp (absolute edx)))))

(define (pop-stack-frame return-address-register)
  (mov (offset ebp -4) return-address-register)
  (mov (offset ebp -8) esp)
  (mov (offset ebp -12) ebp))

(define-error-routine "not_procedure" "not a procedure")
(define-error-routine "argument_count_wrong" "wrong number of arguments")

;; Compiles a procedure into the text segment at a given label.
(define (compile-procedure bodylabel nargs body)
  (text)
  (label bodylabel)
  (compile-procedure-prologue nargs)
  (body)
  (compile-procedure-epilogue))      ; maybe we should just centralize
                                     ; that and jump to it? :)

;; Define a built-in procedure so we can refer to it by label and
;; push-const that label and expect to get a procedure value.
(define (compile-procedure-labeled labelname nargs body)
  (let ((bodylabel (new-label)))
    (rodatum labelname)
    (compile-word procedure-magic)
    (compile-word bodylabel)
    (compile-word "0")                    ; closed over zero artifacts
    (compile-procedure bodylabel nargs body)))


;; Add code to define a global procedure known by a certain global
;; variable name to the header
(define (define-global-procedure symbolname nargs body)
  (add-to-header 
    (lambda () 
      (set-label-prefix symbolname)
      (let ((procedure-value-label (new-label)))
        (define-global-variable symbolname procedure-value-label)
        (compile-procedure-labeled procedure-value-label nargs body)))))

;; Emit code to fetch the Nth argument of the innermost procedure.
(define get-procedure-arg 
  (memo1-asm (lambda (n) 
    (asm-push tos)
    (mov (offset ebp (quadruple n)) tos))))

;; Emit code to mutate it.
(define (set-procedure-arg n)
  (mov tos (offset ebp (quadruple n))))

(define-global-procedure 'procedure? 1
  (lambda () 
    (get-procedure-arg 0)
    (if-not-right-magic-jump procedure-magic "return_false")
    (jmp "return_true")))


;;; Closures and closure handling.
;; If a particular variable is captured by some nested
;; lambda-expression, we heap-allocate that variable.  But that
;; requires knowing which variables are so captured.

;; With respect to a particular lambda-expression, any particular
;; variable can be in one of five categories:
;; - not mentioned;
;; - a stack argument --- one that isn't captured by any inner
;;   lambdas;
;; - a heap argument --- one that is captured by inner lambdas;
;; - an "artifact" --- inherited from some enclosing lexical scope;
;; - a global variable.

;; First, some basic set arithmetic.
(define (set-subtract a b) (filter (lambda (x) (not (memq x b))) a))
(define (set-equal a b) (eq? (set-subtract a b) (set-subtract b a)))
(define (add-if-not-present obj set) (if (memq obj set) set (cons obj set)))
(define (set-union a b) (reduce add-if-not-present a b))
(define (set-intersect a b) (filter (lambda (x) (memq x b)) a))
(define (set-union-all sets) (reduce set-union sets '()))

(assert (set-equal '() '()) "empty set equality")
(assert (set-equal '(a) '(a)) "set equality with one item")
(assert (not (set-equal '(a) '(b))) "set inequality with one item")
(assert (not (set-equal '() '(a))) "set inequality () (a)")
(assert (not (set-equal '(a) '())) "set inequality (a) ()")
(assert (set-equal '(a a) '(a)) "set equality (a a) (a)")
(assert (set-equal '(a b) '(b a)) "set equality sequence varies")
(assert (= (length (add-if-not-present 'a '())) 1) "add to empty set")
(assert (= (length (add-if-not-present 'a '(a))) 1) "redundant add")
(assert (= (length (add-if-not-present 'a '(b))) 2) "nonredundant add")
(define sample-abcd (set-union '(a b c) '(b c d)))
(assert (= (length sample-abcd) 4) "set union")
(assert (memq 'a sample-abcd) "member from set 1")
(assert (memq 'd sample-abcd) "member from set 2")
(assert (not (memq '() sample-abcd)) "nil not in set")

(define (assert-set-equal a b) (assert (set-equal a b) (list 'set-equal a b)))
(assert-set-equal (set-intersect '(a b c) '(b c d)) '(b c))


;; Returns vars captured by some lambda inside expr, i.e. vars that
;; occurs free inside a lambda inside expr.
(define (captured-vars expr) (set-union-all (map free-vars (lambdas expr))))
(define (lambdas expr)
  (cond ((not (pair? expr)) '())       ; non-lists and empty lists
        ;; We don't want the deeper-nested lambdas, just the top-level
        ;; ones.
        ((eq? (car expr) 'lambda) (list expr))
        (else (reduce append (map lambdas expr) '()))))
(define (all-captured-vars expr) (set-union-all (map captured-vars expr)))

;; Returns a list of the vars that are bound by a particular lambda arg list.
(define (vars-bound args) (if (symbol? args) (list args) args))

;; Returns vars that occur free inside a lambda-abstraction with given
;; args and body.
(define (free-vars-lambda args body) 
  (set-subtract (all-free-vars body) (vars-bound args)))

;; Returns vars that occur free inside of expr.
(define (free-vars expr)
  (cond ((symbol? expr) (list expr))
        ((not (pair? expr)) '())
        (else (case (car expr)
                ((lambda)     (free-vars-lambda (cadr expr) (cddr expr)))
                ((%begin %ifeq)
                              (all-free-vars (cdr expr)))
                ((quote)      '())
                ((set!)       (add-if-not-present (cadr expr) 
                                                  (free-vars (caddr expr))))
                (else         (all-free-vars expr))))))
;; Returns vars that occur free inside of any of exprs.
(define (all-free-vars exprs) (set-union-all (map free-vars exprs)))

;; Returns the free vars of a lambda found somewhere in its lexical
;; environment.  This needs access to the lexical environment to
;; distinguish artifacts from globals.
(define (artifacts vars body env) (filter (lambda (x) (assq x env)) 
                                          (free-vars-lambda vars body)))


;; Compiles code to store the requested heap arguments; returns an
;; environment containing them as well as the original contents of the
;; environment.
(define (compile-heap-args heap-args heap-slots-used env)
  (comment "discarding useless value in %eax")
  (pop)
  (compile-heap-args-2 heap-args heap-slots-used env))
(define (compile-heap-args-2 heap-args heap-slots-used env)
  (if (null? heap-args) env
      (let ((var (car heap-args)))
        (begin 
          (comment "move arg from stack to heap: " (symbol->string var))
          (compile-var var env)
          (move-var-to-heap-arg)
          ;; Now we have the heap arg pointer on the stack, hopefully
          ;; in the right place.
          (compile-heap-args-2 (cdr heap-args) (1+ heap-slots-used) 
                               (cons (list var 'heap-pointer
                                           heap-slots-used) env))))))

(define (push-artifacts artifacts) (push-artifacts-2 artifacts 0))
(define (push-artifacts-2 artifacts slotnum)
  (if (null? artifacts) '()
      (let ((var (car artifacts)))
        (comment "fetch artifact from closure: " (number->string slotnum) 
                 " " (symbol->string var))
        ;; 12 skips the magic number, code pointer, and artifact count.
        (asm-push (offset eax (+ 12 (quadruple slotnum))))
        (cons (list var 'heap-pointer slotnum) 
              (push-artifacts-2 (cdr artifacts) (1+ slotnum))))))

(define (push-closure label artifacts env)
  (emit-malloc-n (+ 12 (quadruple (length artifacts))))
  (mov tos ebx)
  (mov (const procedure-magic) (indirect ebx))
  (mov (const label) (offset ebx 4))
  (mov (const (number->string (length artifacts))) (offset ebx 8))
  (store-closure-artifacts ebx 12 artifacts env))

(define (store-closure-artifacts reg off artifacts env)
  (if (not (null? artifacts))
      (begin (get-heap-var (assq (car artifacts) env))
             (mov tos (offset reg off))
             (pop)
             (store-closure-artifacts reg (+ off 4) (cdr artifacts) env))))

;; Heap variable objects are 8 bytes: a magic number and their current value.
(define heap-var-magic "0x1ce11ed")
(define (move-var-to-heap-arg)
  (comment "moving top of stack to newly allocated heap var")
  (emit-malloc-n 8)
  (mov (const heap-var-magic) (indirect tos))
  (asm-pop (offset tos 4)))

;; Some basic unit tests for closure handling.

(define sample-closure-expression    
  '(lambda (a b)
     (lambda (c d)
       (lambda (e f) (+ e f c a)))))

(assert-set-equal (free-vars sample-closure-expression) '(+))
(assert-set-equal (captured-vars sample-closure-expression) '(+))

(define sample-inner-lambda-1 (caddr sample-closure-expression))
(assert-set-equal (free-vars sample-inner-lambda-1) '(a +))
(assert-set-equal (captured-vars sample-inner-lambda-1) '(a +))

(define sample-inner-lambda-2 (caddr sample-inner-lambda-1))
(assert-set-equal (free-vars sample-inner-lambda-2) '(a c +))
(assert-set-equal (captured-vars sample-inner-lambda-2) '(a c +))
(assert-set-equal (artifacts '(e f) (caddr sample-inner-lambda-2)
                             '((c whatever) (d whatever)
                               (a whatever) (b whatever)))
                  '(a c))

;; Some tests for the other cases.
(define sample-quoted-expr '(foo bar '(a b c)))
(assert-set-equal (free-vars sample-quoted-expr) '(foo bar))
(assert-set-equal (captured-vars sample-quoted-expr) '())

(define sample-if-expr '(%ifeq a b c d))
(assert-set-equal (free-vars sample-if-expr) '(a b c d))
(assert-set-equal (captured-vars sample-if-expr) '())

(define sample-begin-expr '(%begin a b c))
(assert-set-equal (free-vars sample-begin-expr) '(a b c))
(assert-set-equal (captured-vars sample-begin-expr) '())

;; In particular, multiple expressions in a lambda body here.
(assert-set-equal (captured-vars 
                   '(%begin (%ifeq x #f (lambda (y) (z a) (y c)) d) e))
                  '(z a c))

(assert-set-equal (captured-vars '(lambda x (x y z))) '(y z))

(define (heap-args varlist body)
  (set-intersect varlist (all-captured-vars body)))

(assert-set-equal '(a) (heap-args (cadr sample-closure-expression)
                                  (cddr sample-closure-expression)))
(assert-set-equal '(c) (heap-args (cadr sample-inner-lambda-1)
                                  (cddr sample-inner-lambda-1)))
(assert-set-equal '() (heap-args (cadr sample-inner-lambda-2)
                                 (cddr sample-inner-lambda-2)))
(assert-set-equal '(message)
                  (heap-args '(message) 
                             '((lambda (message2) 
                                 (display message)
                                 (display message2)
                                 (newline)))))

(assert-set-equal '(a b) (free-vars '(set! a b)))
(assert-set-equal '() (captured-vars '(set! a b)))

;;; Memory management.

(add-to-header
 (lambda () 
   (insn ".bss")
   (label "the_arena")
   (insn ".space 128*1048576")          ; no GC yet!
   (compile-global-variable "arena_pointer" "the_arena")))

;; Emit code to bump a pointer in a register up, if necessary, to be
;; divisible by 4.
(define (align4 reg)
  (add (const "3") reg)
  (asm-and (const "~3") reg))

(define emit-malloc
  (memo0-asm (lambda ()
    (comment "code to allocate memory; untagged number of bytes in %eax")
    (align4 eax)
    (mov (indirect "arena_pointer") ebx)
    (add ebx eax)
    (mov eax (indirect "arena_pointer"))
    (mov ebx eax)
    (comment "now %eax points to newly allocated memory"))))

(define emit-malloc-n 
  (memo1-asm (lambda (n)
    (assert-equal (remainder n 4) 0)
    (let ((ns (number->string n)))
      (comment "allocate bytes:" ns)
      (asm-push tos)
      (mov (indirect "arena_pointer") tos)
      (mov tos ebx)
      (add (const ns) ebx)
      (mov ebx (indirect "arena_pointer"))
      (comment "now %eax points to newly allocated memory")))))


;; XXX still need to implement deallocation and a GC


;;; Strings (on the target)
;; A string consists of the following, contiguous in memory:
;; - 4 bytes of a string magic number 0xbabb1e
;; - 4 bytes of string length "N";
;; - N bytes of string data.
(define string-magic "0xbabb1e")

(define (constant-string-2 contents labelname)
  (rodatum labelname)
  (compile-word string-magic)
  (compile-word (number->string (string-length contents)))
  (ascii contents)
  (text)
  labelname)
;; constant-string: Emit code to represent a constant string.
(define (constant-string contents) (constant-string-2 contents (new-label)))

(define (if-not-right-magic-jump magic destlabel)
  (comment "test whether %eax has magic: " magic)
  (comment "first, ensure that it's a pointer, not something unboxed")
  (test (const "3") tos)              ; test low two bits
  (jnz destlabel)
  (comment "now, test its magic number")
  (cmp (const magic) (indirect tos))
  (jnz destlabel))

(define-error-routine "notstring" "not a string")
(add-to-header (lambda ()
    (label "ensure_string")
    (if-not-right-magic-jump string-magic "notstring")
    (ret)))
;; Emit code to ensure that %eax is a string
(define (ensure-string) (call "ensure_string"))

(define-global-procedure 'string? 1
  (lambda ()
    (get-procedure-arg 0)
    (if-not-right-magic-jump string-magic "return_false")
    (jmp "return_true")))

;; Emit code to pull the string pointer and count out of a string
;; being pointed to and push them on the abstract stack
(define (extract-string)
  (ensure-string)
  (lea (offset tos 8) ebx)              ; string pointer
  (asm-push ebx)
  (mov (offset tos 4) tos))             ; string length

(define-global-procedure 'make-string 1
  (lambda () (get-procedure-arg 0)
             (ensure-integer)
             (comment "we need 8 bytes more than the string length")
             (scheme-to-native-integer tos)
             (add (const "8") tos)
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
             (rep-stosb)
             (comment "now pop and return the address")
             (pop)))

;; pops both index and bound
(define (check-array-bounds)
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
  (pop) (pop))

(define-error-routine "index_out_of_bounds" "array index out of bounds")

(define-global-procedure 'string-set! 3
  (lambda () 
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
    (get-procedure-arg 0)))

(define-global-procedure 'string-ref 2
  (lambda ()
    (comment "string-ref primitive procedure")
    (get-procedure-arg 0)
    (extract-string)
    (get-procedure-arg 1)
    (check-array-bounds)
    (get-procedure-arg 1)
    (scheme-to-native-character tos)
    (comment "get base address of string data from stack")
    (asm-pop ebx)
    (movzbl (indirect (index-register tos ebx 1)) tos)
    (native-to-scheme-character tos)))

;; I was going to write an assembly version of string-blit! here, but
;; with the four necessary bounds checks, it ended up being 33
;; relatively error-prone lines of code.  Since the Scheme version is
;; only four lines of code and is needed anyway for bootstrapping,
;; I'll just stick with that for the time being.

(define (inline-string-length nargs)
  (assert-equal 1 nargs)
  (comment "string-length inlined primitive")
  (extract-string)
  (asm-pop ebx)
  (native-to-scheme-integer tos))

;;; conses
;; They're 12 bytes: magic number, car, cdr.  That's all, folks.

(define cons-magic "0x2ce11ed")
(define (ensure-cons) (call "ensure_cons"))
(add-to-header (lambda () (label "ensure_cons")
                          (if-not-right-magic-jump cons-magic "not_cons")
                          (ret)))
(define-error-routine "not_cons" "not a cons")

(define (inline-car nargs)
  (assert-equal 1 nargs)
  (comment "inlined car")
  (ensure-cons)
  (mov (offset tos 4) tos))
(define (inline-cdr nargs)
  (assert-equal 1 nargs)
  (comment "inlined cdr")
  (ensure-cons)
  (mov (offset tos 8) tos))

;; We define a label here before the procedure prologue so that other
;; asm routines can call cons
(add-to-header (lambda () (text) (label "cons")))
(define-global-procedure 'cons 2
  (lambda ()
    (emit-malloc-n 12)
    (mov (const cons-magic) (indirect tos))
    (mov tos ebx)
    (get-procedure-arg 0)
    (mov tos (offset ebx 4))
    (pop)
    (get-procedure-arg 1)
    (mov tos (offset ebx 8))
    (pop)))
;; Compile a quoted cons cell.
(define (compile-cons car-contents cdr-contents labelname)
  (rodatum labelname)
  (compile-word cons-magic)
  (compile-word car-contents)
  (compile-word cdr-contents)
  (text))

(define-global-procedure 'pair? 1
  (lambda ()
    (get-procedure-arg 0)
    (if-not-right-magic-jump cons-magic "return_false")
    (jmp "return_true")))
(add-to-header 
 (lambda ()
   (label "return_true")
   (mov (const true-value) tos)
   (compile-procedure-epilogue)
   (label "return_false")
   (mov (const false-value) tos)
   (compile-procedure-epilogue)))

;;; Symbols.
;; In-memory structures with magic number "0x1abe1" (for now.)
(define symbol-magic "0x1abe1")
;; XXX refactor these if-not-right-magic-jump predicates
(define-global-procedure 'symbol? 1
  (lambda ()
    (get-procedure-arg 0)
    (if-not-right-magic-jump symbol-magic "return_false")
    (jmp "return_true")))
(add-to-header (lambda () (label "ensure_symbol")
                          (if-not-right-magic-jump symbol-magic "not_symbol")
                          (ret)))
(define-error-routine "not_symbol" "not a symbol")
(define (ensure-symbol) (call "ensure_symbol"))

(define interned-symbol-list '())
(define (intern symbol)
  (interning symbol interned-symbol-list))
(define (interning symbol symlist)
  (cond ((null? symlist) 
         ;; XXX isn't this kind of duplicative with the global variables stuff?
         (set! interned-symbol-list 
               (cons (list symbol (new-label)) interned-symbol-list))
         (car interned-symbol-list))
        ((eq? symbol (caar symlist)) (car symlist))
        (else (interning symbol (cdr symlist)))))
(define (symbol-value symbol) (cadr (intern symbol)))

(define (emit-symbols)
  (comment "symbols")
  (emit-symbols-from "0" interned-symbol-list))
(define (emit-symbols-from last-pointer remaining)
  (if (null? remaining) (emit-symbol-list-header last-pointer)
      (let ((symlabel (car remaining)))
        (comment "symbol: " (symbol->string (car symlabel)))
        (let ((stringlabel (compile-constant (symbol->string (car symlabel)))))
          (rodatum (cdr symlabel))
          (compile-word symbol-magic)
          (compile-word stringlabel)
          (compile-word last-pointer)
          (emit-symbols-from (cdr symlabel) (cdr remaining))))))
(define (emit-symbol-list-header last-pointer)
  (section ".data")
  (label "symbol_table")
  (compile-word last-pointer))

(define (inline-symbol->string nargs)
  (assert-equal 1 nargs)
  (ensure-symbol)
  (mov (offset tos 4) tos))

;; XXX maybe this could use the normal string=?, or vice versa?
(define-global-procedure 'string->symbol 1
  (lambda ()
    (get-procedure-arg 0)
    (extract-string)
    (comment "now string length is in %eax and string data pointer at (%esp)")
    (mov (indirect "symbol_table") ebx)
    (label "string_to_symbol_loop")
    (test ebx ebx)
    (jz "intern_new_symbol")
    (comment "fetch pointer to string value")
    (mov (offset ebx 4) edx)
    (comment "fetch string length")
    (mov (offset edx 4) ecx)
    (cmp ecx eax)
    (jnz "wrong_symbol_thanks_for_playing")
    (comment "fetch string pointer")
    (lea (offset edx 8) esi)
    (mov nos edi)
    (repe-cmpsb)
    (jnz "wrong_symbol_thanks_for_playing")
    (comment "found the right symbol")
    (pop)
    (mov ebx tos)
    (jmp "string_symbol_return")
    (label "wrong_symbol_thanks_for_playing")
    (comment "get address of next symbol")
    (mov (offset ebx 8) ebx)
    (jmp "string_to_symbol_loop")
    (label "intern_new_symbol")
    (comment "get string pointer")
    (get-procedure-arg 0)
    (comment "symbols are 12 bytes")
    (emit-malloc-n 12)
    (mov (const symbol-magic) (indirect tos))
    (comment "store string pointer for new symbol")
    (mov nos ebx)
    (mov ebx (offset tos 4))
    (mov (indirect "symbol_table") ebx)
    (mov ebx (offset tos 8))
    (mov tos (indirect "symbol_table"))
    (label "string_symbol_return")))    

;;; I/O: input and output.  Putout and Vladimir.

;; Emit code which, given a byte count on top of stack and a string
;; pointer underneath it, outputs the string.
(define (write_2)
  (mov tos edx)                         ; byte count in arg 3
  (asm-pop ecx)                         ; byte string in arg 2
  (mov (const "4") eax)                 ; __NR_write
  (syscall))                            ; return value is in %eax

(define-global-procedure 'display 1
  (lambda () (get-procedure-arg 0)
             (extract-string)
             (comment "fd 1: stdout")
             (mov (const "1") ebx)
             (write_2)
             (mov (const nil-value) tos)))

(define-global-procedure 'current-input-port 0
  (lambda () (comment "We don't have ports right now, so return nil")
             (push-const nil-value)))

;; Buffered byte input from standard input.
(define-global-procedure 'read-char '()
  (lambda () (comment "We don't care about our args.")
             (comment "(maybe somebody passed us (current-input-port))")
             (section ".bss")
             (label "read_char_buffer")
             (insn ".space 1024")
             (label "read_char_buffer_end")
             (section ".data")
             (label "read_char_pointer")
             (compile-word "read_char_buffer")
             (label "read_char_buffer_fill_pointer")
             (compile-word "read_char_buffer")
             (text)
             (label "read_char")
             (mov (indirect "read_char_pointer") eax)
             (cmp (indirect "read_char_buffer_fill_pointer") eax)
             (jnz "return_char_from_buffer")
             (cmp (const "read_char_buffer_end") eax)
             (jnz "call_read_syscall")

             (mov (const "read_char_buffer") eax)
             (mov eax (indirect "read_char_pointer"))
             (mov eax (indirect "read_char_buffer_fill_pointer"))

             (label "call_read_syscall")
             (comment "__NR_read; see asm-i486/unistd.h")
             (mov (const "3") eax)
             (comment "stdin")
             (mov (const "0") ebx)
             (mov (indirect "read_char_buffer_fill_pointer") ecx)
             (mov (const "read_char_buffer_end") edx)
             (sub (indirect "read_char_buffer_fill_pointer") edx)
             (syscall)
             (test eax eax)
             (je "return_eof")
             (jl "report_read_error")
             (add eax (indirect "read_char_buffer_fill_pointer"))
             (mov (indirect "read_char_pointer") eax)

             (label "return_char_from_buffer")
             (mov eax ebx)
             (inc ebx)
             (mov ebx (indirect "read_char_pointer"))
             (movzbl (indirect eax) tos)
             (native-to-scheme-character tos)
             (jmp "read_char_return")
             (label "return_eof")
             (mov (const eof-value) tos)
             (label "read_char_return")))  

(define-error-routine "report_read_error" "read error on stdin")

;; We don't have ports, but we do have a special procedure to print
;; strings on stderr!
(define-global-procedure 'display-stderr 1
  (lambda () (get-procedure-arg 0)
             (extract-string)
             (comment "fd 2: stderr")
             (mov (const "2") ebx)
             (write_2)))
;; System call to exit the program.
(define-global-procedure 'exit 1
  (lambda () (get-procedure-arg 0)
             (ensure-integer)
             (scheme-to-native-integer tos)
             (mov tos ebx)
             (mov (const "1") eax)      ; __NR_exit
             (syscall)))

;;; Integers
(define (tagshift str) (list (number->string str) "<<2"))
(define integer-tag "1")
(define-global-procedure 'integer? 1 
  (lambda () (compile-tag-check-procedure integer-tag)))
(define (tagged-integer int) (list integer-tag " + " (tagshift int)))
(add-to-header (lambda ()
    (label "ensure_integer")
    (test (const "1") tos)
    (jz "not_an_integer")
    (test (const "2") tos)
    (jnz "not_an_integer")
    (ret)))
(define-error-routine "not_an_integer" "not an integer")

(define (ensure-integer) (call "ensure_integer"))
;; XXX this is the only reason that we need equal?
(define (assert-equal a b) 
  (if (not (equal? a b)) (error "not equal" (list a b))))

(define (integer-add nargs)
  (assert-equal 2 nargs)
  (comment "inlined integer add")
  (ensure-integer)
  (swap)
  (ensure-integer)
  (asm-pop ebx)
  (add ebx tos)
  (dec tos))                            ; fix up tag
(define (integer-sub nargs)
  (assert-equal 2 nargs)
  (comment "inlined integer subtract")
  (ensure-integer)
  (swap)
  (ensure-integer)
  (sub tos nos)
  (pop)
  (inc tos))                            ; fix up tag
(define (inline-1+ nargs)
  (assert-equal 1 nargs)
  (comment "1+")
  (ensure-integer)
  (add (const (tagshift 1)) tos))
(define (inline-1- nargs)
  (assert-equal 1 nargs)
  (ensure-integer)
  (add (const (tagshift -1)) tos))

;; Emit code to convert a native integer to a tagged integer.
(define (native-to-scheme-integer reg) (sal reg) (sal reg) (inc reg))
;; Emit code to convert a tagged integer to a native integer.    
(define (scheme-to-native-integer reg) (sar reg) (sar reg))

;; Emit code to divide procedure arg 0 by procedure arg 1
;; This merely zeroes out the tags rather than shifting them off.  The
;; normal tagged representation of an integer N is N*4+1.
;; Unfortunately (N*4+1)/(M*4+1) and (N*4+1) % (M*4+1) don't seem to
;; have particularly nice properties, so we divide (N*4) by (M*4)
;; instead.  (N*4) / (M*4) = N/M, and (N*4) % (M*4) = (N%M) * 4.
;; (Barring overflow.)
(define (emit-division-code)
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
  (idiv ebx))

(define-global-procedure 'remainder 2
  (lambda () (emit-division-code)
             (comment "remainder (<<2) is in %edx")
             (mov edx tos)
             (comment "put the tag back")
             (inc tos)))
(define-global-procedure 'quotient 2
  (lambda () (emit-division-code)
             (native-to-scheme-integer tos)))

(define-global-procedure '< 2
  (lambda ()
    (comment "procedure <: (< x y) returns true if x < y")
    (get-procedure-arg 0)
    (ensure-integer)
    (get-procedure-arg 1)
    (ensure-integer)
    (cmp tos nos)
    (pop)
    (jl "return_true")
    (jmp "return_false")))

;;; Booleans and other misc. types
(define enum-tag "2")
(define (enum-value offset) (list enum-tag " + " (tagshift offset)))
(define nil-value (enum-value 256))
(define true-value (enum-value 257))
(define false-value (enum-value 258))
(define eof-value (enum-value 259))
(define-global-procedure 'eof-object? 1
  (lambda ()
    (get-procedure-arg 0)
    (cmp (const eof-value) tos)
    (je "return_true")
    (jmp "return_false")))

;;; Characters (chars).
;; These are unboxed and use "enum-tag" (2).

;; Emit code to jump if TOS isn't a character.
(define (jump-if-not-char label)
  (test (const "1") tos)
  (jnz label)
  (test (const "2") tos)
  (jz label)
  ;; Intel manual 253666 says, "The comparison is
  ;; performed by subtracting the second operand
  ;; from the first operand and then setting the
  ;; status flags in the same manner as the SUB
  ;; instruction."  Here we're using AT&T syntax, so
  ;; that means "the first operand from the second
  ;; operand", so we expect to set the carry flag
  ;; here.
  (cmp (const (enum-value 256)) tos)
  (jnb label))

;; Emit code to generate an error if TOS isn't a character.
(define (ensure-character) (jump-if-not-char "not_a_character"))
(define-error-routine "not_a_character" "not a character")

(define-global-procedure 'char? 1 
  (lambda ()
    (get-procedure-arg 0)
    (jump-if-not-char "return_false")
    (jmp "return_true")))

;; Emit code to leave an unsigned native character in the register,
;; converting from a tagged character.
(define scheme-to-native-character scheme-to-native-integer)
;; Emit code to convert from an unsigned native character to a tagged
;; character.
(define (native-to-scheme-character reg) (sal reg) (inc reg) (sal reg))

;; Return string to represent character in assembly language.
;; Previously used 'a, 'b, 'c, etc., but that gets error-prone with
;; things like backslash.  Much to my surprise it did work with space,
;; tab, and newline, though...
(define (tagged-character char)
  (list enum-tag " + " (number->string (char->integer char)) "<<2"))

(define (inline-integer->char nargs)
  (assert-equal 1 nargs)
  (inc tos)
  (ensure-character))
(define (inline-char->integer nargs)
  (assert-equal 1 nargs)
  (ensure-character)
  (dec tos))


;;; Global variable handling.

(define global-variable-labels '())
(define global-variables-defined '())

(define (allocate-new-global-variable-label! name)
  (let ((label (new-label)))
    (set! global-variable-labels 
          (cons (cons name label) global-variable-labels))
    label))

;; Return a label representing this global variable, allocating a new
;; one if necessary.
(define (global-variable-label name) 
  (let ((binding (assq name global-variable-labels)))
    (if binding (cdr binding) (allocate-new-global-variable-label! name))))

;; Emit code to create a mutable labeled cell, for example for use as
;; a global variable, with a specific assembly label.
(define (compile-global-variable varlabel initial)
  (section ".data")
  (label varlabel)
  (compile-word initial)
  (text))

;; Emit code to create a mutable labeled cell for use as a global
;; variable, bound to a specific identifier.
(define (define-global-variable name initial)
  (if (assq name global-variables-defined)
      ;; In this case this is a duplicate definition; just silently
      ;; overwrite the original since we have no way to issue a
      ;; warning
      (begin (push-const initial) (set-global-variable name))
      (begin (compile-global-variable (global-variable-label name) initial)
             (set! global-variables-defined 
                   (cons (list name) global-variables-defined)))))

;; Emit code to fetch from a global variable
(define (fetch-global-variable varname)
  (asm-push tos) 
  (mov (indirect (global-variable-label varname)) tos))

;; Emit code to set a global variable
;; XXX should define-handling use this?
(define (set-global-variable varname)
  (mov tos (indirect (global-variable-label varname))))

;; Return a list of undefined global variables.
(define (undefined-global-variables)
  (filter (lambda (pair) (not (assq (car pair) global-variables-defined)))
          global-variable-labels))

;; This runs at the end of compilation to report any undefined
;; globals.  The assumption is that you're recompiling frequently
;; enough that there will normally only be one...
(define (assert-no-undefined-global-variables)
  (if (not (null? (undefined-global-variables)))
      (error "error: undefined global" (undefined-global-variables))
      #t))


;;; Compilation of particular kinds of expressions

(define (compile-quote-3 expr labelname)
  (cond ((string? expr) (constant-string-2 expr labelname))
        ;; Explicit let here to avoid output dependency on argument
        ;; evaluation order.
        ((pair? expr) (let ((compiled-car (compile-constant (car expr))))
                        (compile-cons compiled-car (compile-constant (cdr expr))
                                      labelname)))
        (else (error "unquotable" expr)))
  labelname)
;; Return a thing you can stick into instructions to name the constant.
(define (compile-constant expr)
  (cond ((null? expr)    nil-value)
        ((symbol? expr)  (symbol-value expr))
        ((integer? expr) (tagged-integer expr))
        ((boolean? expr) (if expr true-value false-value))
        ((char? expr)    (tagged-character expr))
        (else            (compile-quote-3 expr (new-label)))))
;; compile-quotable: called for auto-quoted things and (quote ...)
;; exprs
(define (compile-quotable obj env) (push-const (compile-constant obj)))

(define fetch-heap-var-pointer 
 (memo1-asm (lambda (slotnum)
           (comment "fetching heap var pointer " (number->string slotnum))
           ;; below %ebp is return address, %esp, and saved %ebp; so the first
           ;; heap var slot is at ebp - 16, and the next one is at ebp - 20.
           (dup)
           (mov (offset ebp (- -16 (quadruple slotnum))) tos))))

(define-error-routine "not_heap_var" "heap-var indirection to non-heap-var")
(add-to-header (lambda ()
    (label "ensure_heap_var")
    (if-not-right-magic-jump heap-var-magic "not_heap_var")
    (ret)))
;; It should be impossible for a user program to cause this check to
;; fail, but it did help me track down a few compiler bugs early on.
(define (ensure-heap-var) 
  ; (call "ensure_heap_var")
  #f)

(define (fetch-heap-var slotnum)
  (fetch-heap-var-pointer slotnum)
  (comment "fetch current value from the heap")
  (ensure-heap-var)
  (mov (offset tos 4) tos))

(define set-heap-var 
  (memo1-asm (lambda (slotnum)
    (fetch-heap-var-pointer slotnum)
    (ensure-heap-var)
    (mov nos ebx)
    (mov ebx (offset tos 4))
    (pop))))

;; needs more cases for things other than stack variables?
;; XXX move globals here?
(define (get-variable vardefn)
  (case (car vardefn)
    ((stack) (get-procedure-arg (cadr vardefn)))
    ((heap-pointer) (fetch-heap-var (cadr vardefn)))
    (else (error "unexpected var type" (car vardefn)))))
(define (set-variable vardefn)
  (case (car vardefn)
    ((stack) (set-procedure-arg (cadr vardefn)))
    ((heap-pointer) (set-heap-var (cadr vardefn)))
    (else (error "unexpected var type" vardefn))))

;; rather than getting the variable value, it gets the variable's
;; location in the heap.
(define (get-heap-var vardefn)
  (if (eq? (cadr vardefn) 'heap-pointer) 
      (fetch-heap-var-pointer (caddr vardefn))
      (error "trying to fetch the heap var pointer for " vardefn)))

(define (compile-var var env)
  (let ((binding (assq var env)))
    (if binding (get-variable (cdr binding))
        (fetch-global-variable var))))

;; Compile a set! form
(define (compile-set var defn env)
  (compile-expr defn env #f)
  (let ((binding (assq var env)))
    (if binding (set-variable (cdr binding))
        (set-global-variable var))))

;; compile an expression, discarding result, e.g. for toplevel
;; expressions
(define (compile-discarding expr env) (compile-expr expr env #f) (pop))

;; Construct an environment binding the local variables of the lambda
;; to bits of code to fetch them.
(define (lambda-environment env vars idx)
  (if (null? vars) '()
      (cons (list (car vars) 'stack idx)
            (lambda-environment env (cdr vars) (1+ idx)))))

(define (compile-lambda rands env tail?) 
  (let ((vars (car rands)) (body (cdr rands)))
    (let ((varlist (if (symbol? vars) (list vars) vars))
          (nargs (if (symbol? vars) '() (length vars))))
      (let ((artifacts (artifacts varlist body env))
            (jumplabel (new-label))
            (stack-env (lambda-environment env varlist 0))
            (heap-arg-list (heap-args varlist body)))
        ;; Nested let so that output doesn't depend on argument
        ;; evaluation order.
        (let ((proclabel (new-label)))
          (comment "jump past the body of the lambda")
          (jmp jumplabel)
          (if (null? artifacts)
              (begin
                ;; There are no artifacts, so we don't need to create a
                ;; closure.
                (compile-procedure-labeled proclabel nargs
                  (lambda () 
                    ;; But there may be inner closures...
                    (let ((inner-env (compile-heap-args heap-arg-list 0
                                                        stack-env)))
                      (compile-begin body inner-env #t))))
                (label jumplabel)
                ;; And we can just push-const it instead of creating a
                ;; new closure.
                (push-const proclabel))
              (begin
                (compile-procedure proclabel nargs 
                  (lambda ()
                    ;; There may still be inner closures.
                    (let ((artifacts-env (push-artifacts artifacts)))
                      (let ((inner-env (compile-heap-args 
                                        heap-arg-list
                                        (length artifacts) ; follow artifacts
                                        (append artifacts-env stack-env))))
                        (compile-begin body inner-env #t)))))
                (label jumplabel)
                (push-closure proclabel artifacts env))))))))

(define (compile-begin rands env tail?)
  (cond ((null? rands) (push-const nil-value))
        ((null? (cdr rands)) (compile-expr (car rands) env tail?))
        (else (begin (compile-discarding (car rands) env)
                     (compile-begin (cdr rands) env tail?)))))

(define (compile-conditional jump-if-false then else-expr env tail?)
  (let ((falselabel (new-label)))
    ;; Nested let so that output doesn't depend on argument
    ;; evaluation order.
    (let ((endlabel (new-label)))
      (jump-if-false falselabel)
      (compile-expr then env tail?)
      (jmp endlabel)
      (label falselabel)
      (compile-expr else-expr env tail?)
      (label endlabel))))

(define (compile-ifeq rands env tail?)
  (let ((a (car rands))
        (b (cadr rands))
        (then (caddr rands))
        (else-expr (cadddr rands)))
    (comment "%ifeq")
    (compile-conditional (lambda (falselabel)
                           (compile-expr (car rands) env #f)
                           (compile-expr (cadr rands) env #f)
                           (cmp tos nos)
                           (pop) (pop)
                           (jnz falselabel))
                         then
                         else-expr
                         env
                         tail?)))
                         


(define (inline-primitive rator rands env)
  (let ((nargs (compile-args rands env)))
    (case rator
      ((+)      (integer-add nargs))
      ((-)      (integer-sub nargs))
      ((1+)     (inline-1+ nargs))
      ((1-)     (inline-1- nargs))
      ((car)    (inline-car nargs))
      ((cdr)    (inline-cdr nargs))
      ((integer->char) (inline-integer->char nargs))
      ((char->integer) (inline-char->integer nargs))
      ((string-length) (inline-string-length nargs))
      ((symbol->string) (inline-symbol->string nargs))
      (else (error "don't know how to inline" rator)))))

;; if, lambda, quote, and set! are the standard Scheme set of
;; primitive special forms.
;; XXX this is misleadingly named.  Only actual procedure calls are
;; "combinations".
(define (compile-combination rator rands env tail?)
  (case rator
    ((%begin) (compile-begin rands env tail?))
    ((lambda) (compile-lambda rands env tail?))
    ((quote)  (assert-equal 1 (length rands))
              (compile-quotable (car rands) env))
    ((set!)   (assert-equal 2 (length rands))
              (compile-set (car rands) (cadr rands) env))
    ((+ - 1+ 1- car cdr integer->char char->integer string-length 
      symbol->string)
              (inline-primitive rator rands env))
    ((%ifeq)  (compile-ifeq rands env tail?))
    (else     (let ((nargs (compile-args rands env)))
                (comment "get procedure")
                (compile-expr rator env #f)
                (comment "apply procedure")
                (if tail? (compile-tail-apply nargs)
                    (compile-apply nargs))))))

(define (compile-expr expr env tail?)
  (cond ((pair? expr)   (compile-combination (car expr) (cdr expr) env tail?))
        ((symbol? expr) (compile-var expr env))
        ((or (string? expr) (boolean? expr) (integer? expr) (char? expr))
                        (compile-quotable expr env))
        (else (error "don't know how to compile" expr))))

(define (compile-args-2 args env n)
  (compile-expr (car args) env #f)      ; args are never in tail position
  (1+ n))
;; Compile arguments for a procedure application.  Returns number of
;; arguments compiled.
(define (compile-args args env)
  (if (null? args) 0
      (compile-args-2 args env (compile-args (cdr args) env))))

(define (compile-toplevel-define name body env)
  (define-global-variable name nil-value)
  (comment "compute initial value for global variable")
  (compile-expr body env #f)
  (comment "initialize global variable with value")
  (mov tos (indirect (global-variable-label name)))
  (pop))

(define global-env '())


;;; Macros.

(define macros '())
(define (define-ur-macro name fun)
  (set! macros (cons (list name fun) macros)))

(define (relevant-macro-definition expr)
  (if (pair? expr) (assq (car expr) macros) #f))
(define (macroexpand-1 expr)
  (if (relevant-macro-definition expr) 
      ((cadr (relevant-macro-definition expr)) (cdr expr))
      expr))

;; This is just a sort of test macro to verify that the macro system
;; works.
(define-ur-macro 'begin (lambda (args) (cons '%begin args)))
;; Limited definition of cond.
(define-ur-macro 'cond
  (lambda (args)
    (cond ((null? args) #f)
          ((eq? (caar args) 'else) (cons 'begin (cdar args)))
          (else (list 'if (caar args) (cons 'begin (cdar args))
                      (cons 'cond (cdr args)))))))
(define-ur-macro 'define 
  (lambda (args) 
    (if (pair? (car args)) (list '%define (caar args) 
                                 (cons 'lambda (cons (cdar args) (cdr args))))
        (cons '%define args))))
(define-ur-macro 'let
  (lambda (args)
    (cons (cons 'lambda (cons (map car (car args)) (cdr args)))
          (map cadr (car args)))))
(define-ur-macro 'case
  (lambda (args)
    (cond ((pair? (car args)) 
           ;; Avoid evaluating expression more than once.  XXX unhygienic
           (list 'let (list (list 'case-atom-key (car args)))
                 (cons 'case (cons 'case-atom-key (cdr args)))))
          ((null? (cdr args)) (list 'begin)) ; XXX indeterminate
          ;; XXX here we unwarrantedly assume there's nothing after an
          ;; else clause
          ((eq? (caadr args) 'else) (cons 'begin (cdadr args)))
          (else (list 'if 
                      (if (= (length (caadr args)) 1)
                          (list 'eqv? (car args) (list 'quote (caaadr args)))
                          (list 'memv (car args) (list 'quote (caadr args))))
                      (cons 'begin (cdadr args))
                      (cons 'case (cons (car args) (cddr args))))))))

(define-ur-macro 'or
  (lambda (args)
    (cond ((null? args) #f)
          ((= 1 (length args)) (car args))
          ;; XXX unhygienic
          (else (list 'let (list (list 'or-internal-argument (car args)))
                      (list 'if 'or-internal-argument 'or-internal-argument
                            (cons 'or (cdr args))))))))
(define-ur-macro 'and
  (lambda (args)
    (cond ((null? args) #t)
          ((= 1 (length args)) (car args))
          (else (list 'if (car args) (cons 'and (cdr args)) #f)))))

(define-ur-macro 'if
  (lambda (args)
    (cond ((= 2 (length args)) (list 'if (car args) (cadr args) #f))
          ((not (= 3 (length args))) (error "if needs 2 or 3 args"))
          ((not (pair? (car args))) (cons '%if args))
          (else
           (case (caar args)
             ((eq? eqv? =)
              (list '%ifeq (cadar args) (caddar args) (cadr args) (caddr args)))
             ((null?)
              (list '%ifeq (cadar args) (list 'quote '()) 
                    (cadr args) (caddr args)))
             ((not)
              (list 'if (cadar args) (caddr args) (cadr args)))
             (else
              (cons '%if args)))))))

(define-ur-macro '%if
  (lambda (args) (list '%ifeq (car args) #f (caddr args) (cadr args))))

;; Expand all macros in expr, recursively.
(define (totally-macroexpand expr)
  (cond ((relevant-macro-definition expr) 
         (totally-macroexpand (macroexpand-1 expr)))
        ((not (pair? expr))      expr)
        (else (case (car expr)
                ((quote) expr)
                ((lambda) (cons 'lambda (cons (cadr expr) 
                                              (map totally-macroexpand 
                                                   (cddr expr)))))
                ;; It's harmless to totally-macroexpand set!, if, and
                ;; begin special forms.
                (else (map totally-macroexpand expr))))))

;; tests for macros
(assert-equal (totally-macroexpand 'foo) 'foo)
(assert-equal (totally-macroexpand '(foo a b c)) '(foo a b c))
(assert (relevant-macro-definition '(begin a b c)) "no begin defn")
(assert-equal (totally-macroexpand '(begin a b c)) '(%begin a b c))
(assert-equal (totally-macroexpand '(if a b c)) '(%ifeq a #f c b))
(assert-equal (totally-macroexpand '(if (a) b c)) '(%ifeq (a) #f c b))
(assert-equal (totally-macroexpand '(if (a) b)) '(%ifeq (a) #f #f b))
(assert-equal (totally-macroexpand '(if (not a) b c)) '(%ifeq a #f b c))
(assert-equal (totally-macroexpand '(if (null? a) b c)) '(%ifeq a '() b c))
(assert-equal (totally-macroexpand '(cond ((eq? x 3) 4 '(cond 3)) 
                                          ((eq? x 4) 8)
                                          (else 6 7)))
              '(%ifeq x 3 (%begin 4 '(cond 3))
                   (%ifeq x 4 (%begin 8)
                       (%begin 6 7))))
(assert-equal (totally-macroexpand '(if (= x 0) 1 (if (eqv? x #\q) 2 3)))
                                   '(%ifeq x 0 1 (%ifeq x #\q 2 3)))
(assert-equal (totally-macroexpand '(let () a b c)) '((lambda () a b c)))
(assert-equal (totally-macroexpand '(let ((a 1) (b 2)) a b c))
              '((lambda (a b) a b c) 1 2))
(assert-equal (totally-macroexpand '(and a b c)) 
              '(%ifeq a #f #f (%ifeq b #f #f c)))
(assert-equal (totally-macroexpand '(or a b c))
              (totally-macroexpand
               '(let ((or-internal-argument a)) 
                  (if or-internal-argument or-internal-argument
                      (let ((or-internal-argument b))
                        (if or-internal-argument or-internal-argument
                            c))))))
(assert-equal (totally-macroexpand '(case x ((y) z) (else xxx)))
              '(%ifeq x 'y (%begin z) (%begin xxx)))

;; This test ensures we don't try to macro-expand lambda argument
;; lists.
(assert-equal (totally-macroexpand
               '(let ((cond (car rands)) (then (cadr rands)) 
                      (else (caddr rands)) (falselabel (new-label)) 
                      (endlabel (new-label)))
                  (compile-expr cond env #f)
                  (jump-if-false falselabel)))
              '((lambda (cond then else falselabel endlabel) 
                  (compile-expr cond env #f)
                  (jump-if-false falselabel)
                  ) (car rands) (cadr rands) (caddr rands) 
                    (new-label) (new-label)))


;;; Top-level compilation with macro-expansion.

(define (compile-toplevel expr)
  (compile-toplevel-expanded (totally-macroexpand expr)))
(define (compile-toplevel-expanded expr)
  ;; XXX missing case where it's an atom
  (if (eq? (car expr) '%define) 
      (begin
        (set-label-prefix (cadr expr))
        (compile-toplevel-define (cadr expr) (caddr expr) global-env))
      (compile-discarding expr global-env)))

;;; Parsing: file handling

;; "ungettable" wraps an input stream with a thunk s that you can read
;; a character from (with (s)) or back up by a character (with (s
;; 'unget)).  You pass in a thunk that returns a character when it's
;; called.

;; XXX come up with a better name.  back-up-able?

;; It would be nice if we could map eof objects into 'eof or something
;; to simplify the parsing transition rules, but then we have to have
;; a way to return them so that (read) can return them, but that took
;; this definition from 5 lines to 16.
(define (ungettable thunk)
  (let ((ungot #f) (last #f))
    (lambda cmd (cond ((not (null? cmd)) (set! ungot last))
                      (ungot (let ((result ungot)) (set! ungot #f) result))
                      (else (set! last (thunk)) last)))))

;; read-from-string returns a thunk that returns successive characters
;; of a string, and then 'eof-indicator after the end of the string.

;; SRFI 6 defines a way to use file operations on strings (in this
;; case, using open-input-string), but at least my version of SCM
;; doesn't support it.  But we need something like it for testing.
(define (read-from-string string)
  (let ((pos 0))
    (lambda () (if (= pos (string-length string)) 'eof-indicator
                   (begin (set! pos (1+ pos)) (string-ref string (1- pos)))))))
;; unit tests:
(define sample-sr (read-from-string "foo"))
(assert-equal (sample-sr) #\f)
(assert-equal (sample-sr) #\o)
(assert-equal (sample-sr) #\o)
(assert-equal (sample-sr) 'eof-indicator)
(assert-equal (sample-sr) 'eof-indicator)

(define sample-unget (ungettable (read-from-string "foo")))
(assert-equal (sample-unget) #\f)
(sample-unget 'unget)
(assert-equal (sample-unget) #\f)
(assert-equal (sample-unget) #\o)
(assert-equal (sample-unget) #\o)
(assert-equal (sample-unget) 'eof-indicator)

;; Actual Parsing.

(define (parse s)
  (let ((c (after-wsp s)))
    (if (parse-eof? c) c
        (case c
          (( #\( ) (parse-list s (after-wsp s)))
          (( #\' ) (list 'quote (parse s)))
          (( #\" ) (parse-string-literal s))
          (( #\# ) (parse-hashy-thing s (s)))
          (else (s 'unget) (parse-atom s))))))
(define (parse-list s c)
  (if (parse-eof? c) (error "missing right paren")
      (case c
        (( #\) ) '())
        (( #\. ) (read-dotted-tail s))
        (else (let ((hd (begin (s 'unget) (parse s))))
                (cons hd (parse-list s (after-wsp s))))))))
(define (read-dotted-tail s)
  (let ((rv (parse s)))
    (if (eqv? #\) (after-wsp s)) rv (error "funky dotted list"))))
(define whitespace-chars "\n ")
(define (after-wsp s) (after-wsp-2 s (s)))
(define (after-wsp-2 s c)
  (case c
    ((#\space #\newline #\tab) (after-wsp s))
    (( #\; ) (discard-comment s) (after-wsp s))
    (else c)))
(define (discard-comment s) (if (not (eqv? (s) #\newline)) (discard-comment s)))
(define (parse-atom s) 
  (let ((atom (parse-atom-2 s (s))))
    (if (parsed-number? atom) (string->number (list->string atom))
        (string->symbol (list->string atom)))))
(define (parsed-number? lst)
  (cond ((null? lst) #f)
        ((char-numeric? (car lst)) (all-numeric? (cdr lst)))
        ((eqv? #\+ (car lst)) (nonempty-and-all-numeric? (cdr lst)))
        ((eqv? #\- (car lst)) (nonempty-and-all-numeric? (cdr lst)))
        (else #f)))
(define (nonempty-and-all-numeric? lst)
  (and (not (null? lst)) (all-numeric? lst)))
(define (all-numeric? lst)
  (or (null? lst) (and (char-numeric? (car lst)) (all-numeric? (cdr lst)))))
(define (parse-atom-2 s c)
  (if (parse-eof? c) '()
      (case c
        (( #\space #\newline #\tab #\; #\( #\) #\' #\" ) (s 'unget) '())
        (else (cons c (parse-atom-2 s (s)))))))
(define (parse-string-literal s) (list->string (parse-string-literal-2 s (s))))
(define (parse-string-literal-2 s c)
  (if (parse-eof? c) (error "eof in string")
      (case c 
        (( #\\ ) 
         (let ((next (s)))
           (let ((decoded
                  (case next ((#\n) #\newline) ((#\t) #\tab) (else next))))
             (cons decoded (parse-string-literal-2 s (s))))))
        (( #\" ) 
         '())
        (else
         (cons c (parse-string-literal-2 s (s)))))))
(define (parse-hashy-thing s c)
  (if (parse-eof? c) (error "eof after #")
      (case c
        (( #\t ) #t)
        (( #\f ) #f)
        (( #\\ ) (parse-char-literal s (s)))
        (else (error "Unimplemented #" c)))))
(define (parse-char-literal s c)
  (cond ((parse-eof? c) (error "eof in char literal"))
        ((char-alphabetic? c) (s 'unget) (parse-named-char s))
        (else c)))
(define (parse-named-char s)
  (let ((name (parse-atom-2 s (s))))
    (if (= 1 (length name)) (car name)
        (case (string->symbol (list->string name))
          ((space) #\space)
          ((newline) #\newline)
          ((tab) #\tab)
          (else (error "Unrecognized character name"
                       (string->symbol (list->string name))))))))
                    

(define (parse-string string) (parse (ungettable (read-from-string string))))
(define (read-expr file) (parse (ungettable (lambda () (read-char file)))))
;; Because we can't make a real eof-object portably, we fake it with this:
(define (parse-eof? x) (or (eof-object? x) (eq? x 'eof-indicator)))

;; Unit tests for parsing.  Unfortunately, there's no portable
;; exception system in Scheme, so this doesn't include any tests of
;; error handling!  (Even (error ...) isn't in R5RS.)
(assert-equal (parse-string "()") '())
(assert-equal (parse-string " ()") '())
(assert-equal (parse-string "\n()") '())
(assert-equal (parse-string " ( )") '())
(assert-equal (parse-string ";hi\n(;hi\n)") '())
(assert-equal (parse-string "x ") 'x)
(assert-equal (parse-string "x") 'x)    ; terminated by eof
(assert-equal (parse-string "xyz") 'xyz)
(assert-equal (parse-string "(xyz)") '(xyz))
(assert-equal (parse-string "(x y z)") '(x y z))
(assert-equal (parse-string "(x y . z)") '(x y . z))
(assert-equal (parse-string "(define (1+ x) (+ x 1))")
              '(define (1+ x) (+ x 1)))
(assert-equal 
 (parse-string "(define (filter fn lst)  ; foo\n  (if (null? lst) '()))")
 '(define (filter fn lst) (if (null? lst) (quote ()))))
(parse-string "(char->string (string-ref \"0123456789\"))") ; digit)))")
(assert-equal (parse-string "(char->string (string-ref \"0123456789\" digit)))")
             '(char->string (string-ref "0123456789" digit)))
(assert-equal (parse-string "(foo\"3\"()\"5\")") '(foo "3" () "5"))
(assert-equal (parse-string "(b a #t #f)") '(b a #t #f))
(assert-equal (parse-string "(mov (offset ebp -8) esp)") 
              '(mov (offset ebp -8) esp))
(assert (parse-eof? (parse-string "")) "parsing at end of file")

(assert-equal 
 (parse-string "(#\\a #\\newline #\\tab #\\space #\\( #\\) #\\# #\\\\)")
 '(#\a #\newline #\tab #\space #\( #\) #\# #\\))

(assert-equal (parse-string "\"hello\\n\\tthere\"") "hello\n\tthere")

;;; Library of (a few) standard Scheme procedures defined in Scheme

(define standard-library 
  '(
    ;; basics
    (define (+ a b) (+ a b)) ; uses magic inlining; subset of standard
    (define (- a b) (- a b)) ; uses magic inlining; subset of standard
    (define (car x) (car x))           ; uses magic inlining; standard
    (define (cdr x) (cdr x))           ; uses magic inlining; standard
    (define (1+ x) (1+ x))              ; uses magic inlining
    (define (1- x) (1- x))              ; uses magic inlining
    (define (list . args) args)         ; standard
    (define (length list)               ; standard
      (if (null? list) 0 (1+ (length (cdr list)))))
    (define (assq obj alist)            ; standard
      (cond ((null? alist)          #f)
            ((eq? obj (caar alist)) (car alist))
            (else                   (assq obj (cdr alist)))))
    (define (memq obj list)             ; standard
      (cond ((null? list)         #f)
            ((eq? obj (car list)) list)
            (else                 (memq obj (cdr list)))))
    (define memv memq)                  ; standard
    (define (reduce fn lst init)        
      (if (null? lst) init (fn (car lst) (reduce fn (cdr lst) init))))
    (define (append2 a b) (reduce cons a b))
    (define (append . args) (reduce append2 args '())) ; standard

    ;; identical to standard caar, cdar, etc.
    (define (caar val) (car (car val)))
    (define (cdar val) (cdr (car val)))
    (define (cadr val) (car (cdr val)))
    (define (cddr val) (cdr (cdr val)))
    (define (caddr val) (car (cdr (cdr val))))
    (define (caadr val) (car (car (cdr val))))
    (define (cdadr val) (cdr (car (cdr val))))
    (define (cadar val) (car (cdr (car val))))
    (define (caddar val) (car (cdr (cdr (car val)))))
    (define (cadddr val) (car (cdr (cdr (cdr val)))))
    (define (caaadr val) (car (car (car (cdr val)))))
    (define (not x) (if x #f #t))       ; standard


    ;; string manipulation
    (define (string-length x) (string-length x)) ; uses magic inlining; standard
    (define (symbol->string x)         ; uses magic inlining; standard
      (symbol->string x))
    ;; XXX we could get rid of this if we weren't using it for creating error msgs
    ;; (and now, again, number->string)
    (define (string-append s1 s2)       ; standard
      (let ((buf (make-string (+ (string-length s1) (string-length s2)))))
        (string-blit! s1 0 (string-length s1) buf 0)
        (string-blit! s2 0 (string-length s2) buf (string-length s1))
        buf))
    ;; copies "len" chars from "src" starting at "srcidx" to "dest"
    ;; starting at "destidx"
    (define (string-blit! src srcidx len dest destidx)
      (if (= len 0) #f 
          (begin (string-set! dest destidx (string-ref src srcidx))
                 (string-blit! src (1+ srcidx) (1- len) dest (1+ destidx)))))


    ;; chars
    (define (char-whitespace? c)
      (case c ((#\space #\newline #\tab) #t) (else #f)))
    (define (char<? a b) (< (char->integer a) (char->integer b)))
    (define (char<=? a b) (or (eqv? a b) (char<? a b)))
    (define (char-between? a b c) (and (char<=? a b) (char<=? b c)))
    (define (char-alphabetic? x) (or (char-between? #\A x #\Z) 
                                     (char-between? #\a x #\z)))
    (define (char-numeric? char) (char-between? #\0 char #\9)) ; standard

    ;; equality
    (define (eq? a b) (if (eq? a b) #t #f)) ; uses magic inlining
    (define = eq?)
    ;; because chars are unboxed, char=? is eq?
    (define char=? eq?)
    ;; and all our numbers are unboxed too
    (define eqv? eq?)

    ;; equal? is a little trickier
    (define (equal? a b)
      (cond ((eq? a b) #t)
            ((string? a) (and (string? b) (string=? a b)))
            ((pair? a) (and (pair? b) (equal? (car a) (car b))
                            (equal? (cdr a) (cdr b))))
            (else #f)))

    ;; string=? needs a loop
    (define (string=? a b)
      (and (= (string-length a) (string-length b)) (string=?-2 a b 0)))
    (define (string=?-2 a b idx)
      (or (= idx (string-length a))
          (and (char=? (string-ref a idx) (string-ref b idx))
               (string=?-2 a b (1+ idx)))))


    ;; type tests
    (define (null? x) (if (null? x) #t #f)) ; uses magic inlining
    (define (boolean? x) (if (eq? x #t) #t (eq? x #f)))
    ;; we don't have any other kinds of numbers
    (define number? integer?)    


    ;; list utils
    (define (for-each proc list)   ; subset of standard: one list only
      (if (not (null? list))
          (begin
            (proc (car list))
            (for-each proc (cdr list)))))
    (define (map proc list)        ; subset of standard: one list only
      (if (null? list) '() (cons (proc (car list)) (map proc (cdr list)))))

    (define (reverse lst) (reverse-plus '() lst))
    (define (reverse-plus tail lst) 
      (if (null? lst) tail (reverse-plus (cons (car lst) tail) (cdr lst))))


    ;; conversions
    (define (string->list string)       ; standard
      (string->list-2 string (string-length string) '()))
    (define (string->list-2 string n rest)
      (if (= n 0) rest
          (string->list-2 string (- n 1)
                          (cons (string-ref string (- n 1)) rest))))
    (define (integer->char x) (integer->char x)) ; uses magic inlining; standard
    (define (char->integer x) (char->integer x)) ; uses magic inlining; standard

    (define (list->string lst)
      (list->string-2 (make-string (length lst)) lst 0))
    (define (list->string-2 buf lst idx)
      (if (null? lst) buf
          (begin (string-set! buf idx (car lst))
                 (list->string-2 buf (cdr lst) (1+ idx)))))

    (define (char->string char)
      (let ((buf (make-string 1))) (string-set! buf 0 char) buf))

    (define (string-digit digit) 
      (char->string (string-ref "0123456789" digit)))
    (define (number->string-2 num)
      (if (< num 10) (string-digit num)
          (string-append (number->string-2 (quotient num 10))
                         (string-digit (remainder num 10)))))
    ;; Converts a number into a string of digits.
    (define (number->string num)        ; standard
      (if (< num 0) (string-append "-" (number->string-2 (- 0 num)))
          (number->string-2 num)))

    ;; Converts a string of digits into a number.
    (define (string->number str)
      (if (= (string-length str) 0) (error "string->number of empty string")
          (case (string-ref str 0)
            (( #\+ )      (string->number-2 str 1 0))
            (( #\- ) (- 0 (string->number-2 str 1 0)))
            (else         (string->number-2 str 0 0)))))
    (define (string->number-2 str idx sofar)
      (if (= idx (string-length str)) sofar
          (let ((c (string-ref str idx)))
            (if (not (char-between? #\0 c #\9)) 
                (error "non-numeric char" c str)
                (string->number-2
                 str
                 (1+ idx)
                 (+ (10* sofar) (- (char->integer c) (char->integer #\0))))))))
    (define (10* x) (+ (8* x) (2* x)))
    (define (2* x) (+ x x))
    (define (8* x) (2* (2* (2* x))))


    ;; etc.
    (define (newline) (display "\n"))
    (define (error . args)
      (display-stderr "error: ")
      (for-each (lambda (arg) 
                  (wthunk arg display-stderr)
                  (display-stderr " ")) args)
      (display-stderr "\n")
      (exit 1))
    (define (escape-char char dangerous escapes) ; duplicated in stdlib
      (cond ((null? dangerous) (char->string char))
            ((char=? char (string-ref (car dangerous) 0))
             (car escapes))
            (else (escape-char char (cdr dangerous) (cdr escapes)))))
    (define (escape string idx dangerous escapes) ; duplicated in stdlib
      (if (= idx (string-length string)) '()
          (cons (escape-char (string-ref string idx) dangerous escapes)
                (escape string (1+ idx) dangerous escapes))))
    ;; Escape the three necessary characters.  duplicated in stdlib
    (define (backslash string) (escape string 0 '("\\"   "\n"  "\"") 
                                                '("\\\\" "\\n" "\\\"")))
    (define (write x) (wthunk x display))
    (define (wthunk x display)
      (cond ((string? x) (wstring x display))
            ((or (pair? x) (null? x)) (display "(") (wlist x display))
            ((symbol? x) (display (symbol->string x)))
            ((number? x) (display (number->string x)))
            ((eq? x #t) (display "#t"))
            ((eq? x #f) (display "#f"))
            ((eq? x #\newline) (display "#\\newline"))
            ((eq? x #\space) (display "#\\space"))
            ((eq? x #\tab) (display "#\\tab"))
            ((char? x) (display "#\\") (display (char->string x)))
            (else (error "don't know how to write" x))))
    (define (wstring x pr) (pr "\"") (for-each pr (backslash x)) (pr "\""))
    (define (wlist x pr) 
      (cond ((null? x)
             (pr ")"))
            ((pair? x)
             (wthunk (car x) pr) 
             (if (not (null? (cdr x))) (pr " "))
             (wlist (cdr x) pr))
            (else
             (pr ". ")
             (wthunk x pr)
             (pr ")"))))
))

;;; Main Program

(define (compile-program body)
  (stuff-to-put-in-the-header)

  (global-label "_start")             ; allow compiling with -nostdlib
  (insn ".weak _start")         ; but also allow compiling with stdlib
  (global-label "main")         ; with entry point of main, not _start
  (mov (const "0x610ba1") ebp)          ; global-scope ebp

  (for-each compile-toplevel standard-library)
  (comment "(end of standard library prologue)")

  (body)

  (mov (const "1") eax)             ; __NR_exit
  (mov (const "0") ebx)             ; exit code
  (syscall)
  (emit-symbols)
  (assert-no-undefined-global-variables))

(define (read-compile-loop)
  (let ((expr (read-expr (current-input-port))))
    (if (not (eof-object? expr))
        (begin (compile-toplevel expr)
               (read-compile-loop)))))

(compile-program read-compile-loop)
