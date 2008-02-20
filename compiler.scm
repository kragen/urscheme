;;; UrScheme: A self-hosting compiler for a subset of R5RS Scheme to x86 asm
;; Kragen Javier Sitaker, 2008-01-03 through 10

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
;; D eq?, pair?, null?, symbol?, integer?, boolean?, string?, procedure? 
;; - char?
;; D if (with three arguments)
;; D lambda (with fixed numbers of arguments or with a single argument
;;   that gets bound to the argument list (lambda <var> <body>)
;; D begin
;; D global variables
;; D lexically-scoped local variables
;; - nested scopes and closures
;; - set! for global and local variables
;; D top-level define of a variable (not a function)
;; - read, for proper and improper lists, symbols, strings, integers,
;;   #t and #f, and '
;; - consequently symbols need to store their strings, and we need
;;   string->symbol; other parts of the compiler use symbol->string
;; - eof-object?
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
;; - error
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
;; D current-input-port
;; D read-char
;; - char-alphabetic?
;; - and
;; - symbol->string, string->symbol
;;    1D make a compile-time alist of symbol labels
;;    2D emit them just as magic words at the end of the file
;;    3. change symbols to be merely pointers to those things
;;    4. add pointers to strings to the symbols
;;    5. add symbol->string
;;    6. add string->symbol
;; - string->number
;; D list->string (already have string->list)
;; - char?
;; - set!
;; - error

;; There were a bunch of parts of standard Scheme that I implemented
;; at the top of the compiler, which was a little bit silly --- any
;; program to be compiled by this compiler would either have to forgo
;; using those same facilities, or reimplement them itself.

;; Now I have moved them into a prelude called "standard-library" that
;; gets compiled before the user's program, which considerably expands
;; the subset of Scheme supported without adding any complexity to the
;; overall system.

;;; Not implemented:
;; - call/cc, dynamic-wind
;; - macros, quasiquote
;; - most of arithmetic
;; - vectors
;; - some of the language syntax ` , ,@
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

;; Note: these currently cause a compile error because they are also
;; in our standard library.  The names come from SBCL.  XXX I don't
;; know if they are in Common Lisp.
(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (filter fn lst)  ; this must exist in r5rs but I can't find it
  (if (null? lst) '()
      (let ((first (car lst)) (rest (filter fn (cdr lst))))
        (if (fn first) (cons first rest) rest))))

(define (char->string char) 
  (let ((buf (make-string 1))) (string-set! buf 0 char) buf))
(define (string-digit digit) (char->string (string-ref "0123456789" digit)))
(define (number->string-2 num tail)
  (if (= num 0) tail
      (number->string-2 (quotient num 10)
                        (string-append (string-digit (remainder num 10)) 
                                       tail))))
;; Converts a number into a string of digits.
;; XXX move into standard library!
(define (number->string num)                  ; same as standard
  (cond ((= num 0) "0")
        ((< num 0) (string-append "-" (number->string-2 (- 0 num) "")))
        (else (number->string-2 num ""))))

;; Boy, it sure causes a lot of hassle that Scheme has different types
;; for strings and chars.

(define (string-idx-2 string char idx)
  (cond ((= idx (string-length string)) #f)
        ((char=? (string-ref string idx) char) idx)
        (else (string-idx-2 string char (1+ idx)))))
;; returns #f or index into string
(define (string-idx string char) (string-idx-2 string char 0))

;;; Basic Assembly Language Emission

;; emit: output a line of assembly by concatenating the strings in an
;; arbitrarily nested list structure
(define (emit . stuff) (emit-inline stuff) (newline))
(define (emit-inline stuff)
  (cond ((null? stuff) #t)
        ((pair? stuff) (emit-inline (car stuff)) (emit-inline (cdr stuff)))
        ((string? stuff) (display stuff))
        (else (error (list "emitting" stuff)))))

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
(define jnb (onearg "jnb"))       (define jg (onearg "jg"))
(define js (onearg "js"))
(define call (onearg "call"))     (define int (onearg "int"))
(define inc (onearg "inc"))       (define dec (onearg "dec"))
(define idiv (onearg "idiv"))
;; These have two-arg forms too, but I'm not using them.
(define sal (onearg "sal"))       (define sar (onearg "sar"))

;; Currently only using two zero-argument instructions:
(define (ret) (insn "ret"))
(define (repstosb) (insn "rep stosb"))

;; Registers:
(define eax "%eax")  (define ebx "%ebx")  
(define ecx "%ecx")  (define edx "%edx")
(define ebp "%ebp")  (define esp "%esp")
(define edi "%edi")
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
(define (set-label-prefix new-prefix) 
  ;; XXX we should avoid duplicates
  (set! label-prefix (cons "_"
                           (escape (symbol->string new-prefix) 0 
                                   ;; XXX incomplete list
                                   '("+"    "-" "="  "?" ">"  "<"  "!")
                                   '("Plus" "_" "Eq" "P" "Gt" "Lt" "Bang"))))
  (set! constcounter 0))
(define (new-label)
  (set! constcounter (1+ constcounter))
  (list label-prefix "_" (number->string constcounter)))

;; stuff to output a Lisp string safely for assembly language
(define dangerous '("\\" "\n" "\""))
(define escapes '("\\\\" "\\n" "\\\""))
(define (escape-char char dangerous escapes)
  (cond ((null? dangerous) (char->string char))
        ((char=? char (string-ref (car dangerous) 0))
         (car escapes))
        (else (escape-char char (cdr dangerous) (cdr escapes)))))
(define (escape string idx dangerous escapes)
  (if (= idx (string-length string)) '()
      (cons (escape-char (string-ref string idx) dangerous escapes)
            (escape string (1+ idx) dangerous escapes))))
;; Represent a string appropriately for the output assembly language file.
(define (asm-represent-string string)
  (list "\"" (escape string 0 dangerous escapes) "\""))

(define (ascii string) (insn ".ascii " (asm-represent-string string)))

;; emit a prologue for a datum to be assembled into .rodata
(define (rodatum labelname)
  (rodata)
  (comment "align pointers so they end in binary 00")
  (insn ".align 4")
  (label labelname))

(define (compile-word contents) (insn ".int " contents))

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
(define (compile-apply nargs)
  (ensure-procedure)
  (mov (offset tos 4) ebx)              ; address of actual procedure
  (mov (const (number->string nargs)) edx)
  (call (absolute ebx)))
(define (compile-tail-apply nargs)
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
  (jmp (absolute ebx)))
(define (copy-args basereg nargs i)
  (if (= nargs i) '()
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
    
(define (compile-procedure-prologue nargs)
  (if (null? nargs) (compile-variadic-prologue)
      (begin
        (comment "compute desired %esp on return in %ebx and push it")
        (lea (offset (index-register esp edx 4) 4) ebx)
        (asm-push ebx)

        (asm-push ebp)                  ; save old %ebp
        (lea (offset esp 12) ebp) ; 12 bytes to skip saved %ebp, %ebx, %eip

        (cmp (const (number->string nargs)) edx)
        (jnz "argument_count_wrong"))))
(define (compile-procedure-epilogue)
  (comment "procedure epilogue")
  (comment "get return address")
  (pop-stack-frame edx)
  (jmp (absolute edx)))

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
(define (get-procedure-arg n) 
  (asm-push tos)
  (mov (offset ebp (quadruple n)) tos))

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
(define (set-union a b) (if (null? b) a 
                            (add-if-not-present (car b) (set-union (cdr b) a))))
(define (set-intersect a b) (filter (lambda (x) (memq x b)) a))

(define (assert x why) (if (not x) (error "surprise! error" why) '()))
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
(define (captured-vars expr)
    (if (not (pair? expr)) '()
        (case (car expr)
          ((lambda)   (free-vars-lambda (cadr expr) (cddr expr)))
          ((if %begin) (all-captured-vars (cdr expr)))
          ((quote)    '())
          (else       (all-captured-vars expr)))))

;; Returns true if var is captured by a lambda inside any of exprs.
(define (all-captured-vars exprs) 
  (if (null? exprs) '()
      (set-union (captured-vars (car exprs))
                 (all-captured-vars (cdr exprs)))))

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
                ((lambda)    (free-vars-lambda (cadr expr) (cddr expr)))
                ((if %begin) (all-free-vars (cdr expr)))
                ((quote)     '())
                (else        (all-free-vars expr))))))
;; Returns vars that occur free inside of any of exprs.
(define (all-free-vars exprs) (if (null? exprs) '()
                                  (set-union (free-vars (car exprs))
                                             (all-free-vars (cdr exprs)))))

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
      ;; XXX compile-var doesn't need a tail? argument!
      (let ((var (car heap-args)))
        (begin 
          (comment "move arg from stack to heap: " (symbol->string var))
          (compile-var var env #f)
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
  (push-const (tagged-integer (+ 12 (quadruple (length artifacts)))))
  (emit-malloc)
  (mov tos ebx)
  (mov (const procedure-magic) (indirect ebx))
  (mov (const label) (offset ebx 4))
  (mov (const (number->string (length artifacts))) (offset ebx 8))
  (store-closure-artifacts ebx 12 artifacts env))

(define (store-closure-artifacts reg off artifacts env)
  (if (null? artifacts) '()
      (begin (get-heap-var (assq (car artifacts) env))
             (mov tos (offset reg off))
             (pop)
             (store-closure-artifacts reg (+ off 4) (cdr artifacts) env))))

;; Heap variable objects are 8 bytes: a magic number and their current value.
(define heap-var-magic "0x1abe11ed")
(define (move-var-to-heap-arg)
  (comment "moving top of stack to newly allocated heap var")
  (push-const (tagged-integer 8))
  (emit-malloc)
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

(define sample-if-expr '(if a b c))
(assert-set-equal (free-vars sample-if-expr) '(a b c))
(assert-set-equal (captured-vars sample-if-expr) '())

(define sample-begin-expr '(if a b c))
(assert-set-equal (free-vars sample-begin-expr) '(a b c))
(assert-set-equal (captured-vars sample-begin-expr) '())

;; In particular, multiple expressions in a lambda body here.
(assert-set-equal (captured-vars '(begin (if x (lambda (y) (z a) (y c)) d) e))
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

(define (emit-malloc)
  (comment "code to allocate memory; tagged number of bytes in %eax")
  (ensure-integer)
  (scheme-to-native-integer eax)
  (align4 eax)
  (mov (indirect "arena_pointer") ebx)
  (add ebx eax)
  (mov eax (indirect "arena_pointer"))
  (mov ebx eax)
  (comment "now %eax points to newly allocated memory"))

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
             (pop)))

(define (check-array-bounds )
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

(define-global-procedure 'string-length 1
  (lambda ()
    (comment "string-length primitive procedure")
    (get-procedure-arg 0)
    (extract-string)
    (asm-pop ebx)
    (native-to-scheme-integer tos)))

;;; conses
;; They're 12 bytes: magic number, car, cdr.  That's all, folks.

(define cons-magic "0x2ce11ed")
(define (ensure-cons) (call "ensure_cons"))
(add-to-header (lambda () (label "ensure_cons")
                          (if-not-right-magic-jump cons-magic "not_cons")
                          (ret)))
(define-error-routine "not_cons" "not a cons")
(define-global-procedure 'car 1
  (lambda ()
    (get-procedure-arg 0)
    (ensure-cons)
    (mov (offset tos 4) tos)))
(define-global-procedure 'cdr 1
  (lambda ()
    (get-procedure-arg 0)
    (ensure-cons)
    (mov (offset tos 8) tos)))
;; We define a label here before the procedure prologue so that other
;; asm routines can call cons
(add-to-header (lambda () (text) (label "cons")))
(define-global-procedure 'cons 2
  (lambda ()
    (push-const (tagged-integer 12))
    (emit-malloc)
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
;; (XXX In transition from being tagged numbers.)
(define symbol-tag "3")
(define symbol-magic "0x1abe1")
(define-global-procedure 'symbol? 1
  (lambda () (compile-tag-check-procedure symbol-tag)))
(define interned-symbol-list '())
(define (intern symbol)
  (interning symbol interned-symbol-list))
(define (interning symbol symlist)
  (cond ((null? symlist) 
         ;; XXX isn't this kind of duplicative with the global variables stuff?
         (set! interned-symbol-list 
               (cons (list symbol (new-label)) interned-symbol-list))
         (length interned-symbol-list))
        ((eq? symbol (caar symlist)) (length symlist))
        (else (interning symbol (cdr symlist)))))
(define (symbol-value symbol) (list "3 + " (tagshift (intern symbol))))

(define (emit-symbols)
  (comment "symbols")
  (rodata)
  (for-each (lambda (symlabel)
              (comment "symbol: " (symbol->string (car symlabel)))
              (label (cdr symlabel))
              (compile-word symbol-magic))
            interned-symbol-list))

;;; I/O: input and output.  Putout and Vladimir.

;; Emit code which, given a byte count on top of stack and a string
;; pointer underneath it, outputs the string.
(define (write_2)
  (mov tos edx)                         ; byte count in arg 3
  (asm-pop ecx)                         ; byte string in arg 2
  (mov (const "4") eax)                 ; __NR_write
  (syscall))                            ; return value is in %eax

;; Emit code to output a string.
;; XXX this needs to have a reasonable return value, and it doesn't!
(define (target-display) 
  (extract-string)
  (comment "fd 1: stdout")
  (mov (const "1") ebx)
  (write_2))
;; Emit code to output a newline.
(define (target-newline)
  (push-const "newline_string")
  (target-display))
(add-to-header (lambda () (constant-string-2 "\n" "newline_string")))

(define-global-procedure 'display 1
  (lambda () (get-procedure-arg 0)
             (target-display)))
(define-global-procedure 'newline 0 target-newline)
(define-global-procedure 'eq? 2 
  (lambda () (get-procedure-arg 0)
             (get-procedure-arg 1)
             (target-eq?)))

(define-global-procedure 'current-input-port 0
  (lambda () (comment "We don't have ports right now, so return nil")
             (push-const nil-value)))

(define-global-procedure 'read-char '()
  (lambda () (comment "We don't care about our args.")
             (comment "(maybe somebody passed us (current-input-port))")
             (section ".data")
             (label "read_char_buffer")
             (compile-word "0")
             (text)
             (comment "__NR_read; see asm-i486/unistd.h")
             (mov (const "3") eax)
             (comment "stdin")
             (mov (const "0") ebx)
             (mov (const "read_char_buffer") ecx)
             (mov (const "1") edx)
             (syscall)
             (test eax eax)
             (je "return_eof")
             (movzbl (indirect "read_char_buffer") tos)
             (native-to-scheme-character tos)
             (jmp "read_char_return")
             (label "return_eof")
             (mov (const eof-value) tos)
             (label "read_char_return")))  

;;; Other miscellaneous crap that needs reorganizing

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
;; XXX I just added equal? to the required subset of the language
(define (assert-equal a b) (if (equal? a b) #t (error "not equal" (list a b))))
;; Emit code to add NOS to TOS; assumes they're already tag-checked
(define (emit-integer-addition) (asm-pop ebx)
                                (add ebx tos)
                                (dec tos)) ; fix up tag

(define (integer-add rands env tail?)
  (comment "integer add operands")
  (assert-equal 2 (compile-args rands env))
  (comment "now execute integer add")
  (ensure-integer)
  (swap)
  (ensure-integer)
  (emit-integer-addition))
(define (integer-sub rands env tail?)
  (comment "integer subtract operands")
  (assert-equal 2 (compile-args rands env))
  (comment "now execute integer subtract")
  (ensure-integer)
  (swap)
  (ensure-integer)
  (sub tos nos)
  (pop)
  (inc tos))                            ; fix up tag

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
    (jg "return_false")
    (jmp "return_true")))

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

(define (jump-if-false label)
  (cmp (const false-value) tos)
  (pop)
  (je label))

;; Emit code to generate an error if TOS isn't a character.
(define (ensure-character) 
  (test (const "1") tos)
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
  (jnb "not_a_character"))

(define-error-routine "not_a_character" "not a character")

;; Emit code to leave an unsigned native character in the register,
;; converting from a tagged character.
(define scheme-to-native-character scheme-to-native-integer)
;; Emit code to convert from an unsigned native character to a tagged
;; character.
(define (native-to-scheme-character reg) (sal reg) (inc reg) (sal reg))

(define (tagged-character char)
  (list enum-tag " + '" (char->string char) "<<2"))

;; Emit code to push a boolean in place of the top two stack items.
;; It will be #t if they are equal, #f if they are not.
(define (target-eq?)
  (let ((label1 (new-label)) (label2 (new-label)))
    (asm-pop ebx)
    (cmp ebx tos)
    (je label1)
    (mov (const false-value) tos)
    (jmp label2)
    (label label1)
    (mov (const true-value) tos)
    (label label2)))


;;; Global variable handling.

(define global-variable-labels '())
(define global-variables-defined '())

(define (add-new-global-variable-binding! name label)
  (set! global-variable-labels 
        (cons (cons name label) global-variable-labels))
  label)
(define (allocate-new-global-variable-label! name) 
  (add-new-global-variable-binding! name (new-label)))

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
  (if (assq name global-variables-defined) (error "double define" name)
      (begin (compile-global-variable (global-variable-label name) initial)
             (set! global-variables-defined 
                   (cons (list name) global-variables-defined)))))

;; Emit code to fetch from a named global variable.
(define (fetch-global-variable varname)
  (asm-push tos) 
  (mov (indirect varname) tos))

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
        ((pair? expr) (compile-cons (compile-quote-2 (car expr))
                                    (compile-quote-2 (cdr expr))
                                    labelname))
        (else (error "unquotable" expr)))
  labelname)
(define (compile-quote-2 expr)
  (cond ((null? expr)    nil-value)
        ((symbol? expr)  (symbol-value expr))
        ((integer? expr) (tagged-integer expr))
        ((boolean? expr) (if expr true-value false-value))
        ((char? expr)    (tagged-character expr))
        (else            (compile-quote-3 expr (new-label)))))
;; compile-quotable: called for auto-quoted things and (quote ...)
;; exprs
(define (compile-quotable obj env) (push-const (compile-quote-2 obj)))

(define (fetch-heap-var-pointer slotnum)
  (comment "fetching heap var pointer " (number->string slotnum))
  ;; below %ebp is return address, %esp, and saved %ebp; so the first
  ;; heap var slot is at ebp - 16, and the next one is at ebp - 20.
  (dup)
  (mov (offset ebp (- -16 (quadruple slotnum))) tos))

(define-error-routine "not_heap_var" "heap-var indirection to non-heap-var")
(add-to-header (lambda ()
    (label "ensure_heap_var")
    (if-not-right-magic-jump heap-var-magic "not_heap_var")
    (ret)))
(define (ensure-heap-var) (call "ensure_heap_var"))

(define (fetch-heap-var slotnum)
  (fetch-heap-var-pointer slotnum)
  (comment "now fetching current value from the heap")
  (ensure-heap-var)
  (mov (offset tos 4) tos))

;; needs more cases for things other than stack variables?
(define (get-variable vardefn)
  (case (car vardefn)
    ((stack) (get-procedure-arg (cadr vardefn)))
    ((heap-pointer) (fetch-heap-var (cadr vardefn)))
    (else (error (list "unexpected var type" (car vardefn))))))

;; rather than getting the variable value, it gets the variable's
;; location in the heap.
(define (get-heap-var vardefn)
  (if (eq? (cadr vardefn) 'heap-pointer) 
      (fetch-heap-var-pointer (caddr vardefn))
      (error "trying to fetch the heap var pointer for " vardefn)))

(define (compile-var var env tail?)
  (let ((binding (assq var env)))
    (if binding (get-variable (cdr binding))
        (fetch-global-variable (global-variable-label var)))))

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
            (proclabel (new-label))
            (jumplabel (new-label))
            (stack-env (lambda-environment env varlist 0))
            (heap-arg-list (heap-args varlist body)))
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
              (push-closure proclabel artifacts env)))))))

(define (compile-begin rands env tail?)
  (cond ((null? rands) (push-const "31")) ; XXX do something reasonable
        ((null? (cdr rands)) (compile-expr (car rands) env tail?))
        ;; hey, we can avoid discarding the results from
        ;; intermediate expressions if we're at the top level of a
        ;; function...
        (else
         (begin (if tail? (compile-expr (car rands) env #f)
                    (compile-discarding (car rands) env))
                (compile-begin (cdr rands) env tail?)))))

(define (compile-if rands env tail?)
  (if (not (= (length rands) 3))
      (error "if arguments length " (length rands) " != 3")
      (let ((cond (car rands)) (then (cadr rands)) (else (caddr rands))
            (falselabel (new-label)) (endlabel (new-label)))
        (compile-expr cond env #f)
        (jump-if-false falselabel)
        (compile-expr then env tail?)
        (jmp endlabel)
        (label falselabel)
        (compile-expr else env tail?)
        (label endlabel))))


;; if, lambda, quote, and set! are the standard Scheme set of
;; primitive special forms.
(define (compile-combination rator rands env tail?)
  (case rator
    ((%begin) (compile-begin rands env tail?))
    ((if)     (compile-if rands env tail?))
    ((lambda) (compile-lambda rands env tail?))
    ((quote)  (assert-equal 1 (length rands))
              (compile-quotable (car rands) env))
    ((+)      (integer-add rands env tail?))
    ((-)      (integer-sub rands env tail?))
    (else     (let ((nargs (compile-args rands env)))
                (comment "get the procedure")
                (compile-expr rator env #f)
                (comment "now apply the procedure")
                (if tail? (compile-tail-apply nargs)
                    (compile-apply nargs))))))

(define (compile-expr expr env tail?)
  (cond ((pair? expr)   (compile-combination (car expr) (cdr expr) env tail?))
        ((symbol? expr) (compile-var expr env tail?))
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
(define (define-macro name fun)
  (set! macros (cons (list name fun) macros)))

(define (relevant-macro-definition expr)
  (if (pair? expr) (assq (car expr) macros) #f))
(define (macroexpand-1 expr)
  (if (relevant-macro-definition expr) 
      ((cadr (relevant-macro-definition expr)) (cdr expr))
      expr))

;; This is just a sort of test macro to verify that the macro system
;; works.
(define-macro 'begin (lambda (args) (cons '%begin args)))
;; Limited definition of cond.
(define-macro 'cond
  (lambda (args)
    (cond ((null? args) #f)
          ((eq? (caar args) 'else) (cons 'begin (cdar args)))
          (else (list 'if (caar args) (cons 'begin (cdar args))
                      (cons 'cond (cdr args)))))))
(define-macro 'define 
  (lambda (args) 
    (if (pair? (car args)) (list '%define (caar args) 
                                 (cons 'lambda (cons (cdar args) (cdr args))))
        (cons '%define args))))
(define-macro 'let
  (lambda (args)
    (cons (cons 'lambda (cons (map car (car args)) (cdr args)))
          (map cadr (car args)))))
(define-macro 'case
  (lambda (args)
    (cond ((pair? (car args)) 
           ;; Avoid evaluating expression more than once.  XXX unhygienic
           (list 'let (list (list 'case-atom-key (car args)))
                 (cons 'case (cons 'case-atom-key (cdr args)))))
          ((null? (cdr args)) (list 'begin)) ; XXX indeterminate
          ;; XXX here we unwarrantedly assume there's nothing after an
          ;; else clause
          ((eq? (caadr args) 'else) (cons 'begin (cdadr args)))
          (else (list 'if (list 'memv (car args) (list 'quote (caadr args)))
                      (cons 'begin (cdadr args))
                      (cons 'case (cons (car args) (cddr args))))))))

(define-macro 'or
  (lambda (args)
    (cond ((null? args) #f)
          ((= 1 (length args)) (car args))
          ;; XXX unhygienic
          (else (list 'let (list (list 'or-internal-argument (car args)))
                      (list 'if 'or-internal-argument 'or-internal-argument
                            (cons 'or (cdr args))))))))

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
                (else (map totally-macroexpand expr))))))

;; tests for macros
(assert-equal (totally-macroexpand 'foo) 'foo)
(assert-equal (totally-macroexpand '(if a b c)) '(if a b c))
(assert (relevant-macro-definition '(begin a b c)) "no begin defn")
(assert-equal (totally-macroexpand '(begin a b c)) '(%begin a b c))
(assert-equal (totally-macroexpand '(cond ((eq? x 3) 4 '(cond 3)) 
                                          ((eq? x 4) 8)
                                          (else 6 7)))
              '(if (eq? x 3) (%begin 4 '(cond 3))
                   (if (eq? x 4) (%begin 8)
                       (%begin 6 7))))
(assert-equal (totally-macroexpand '(let () a b c)) '((lambda () a b c)))
(assert-equal (totally-macroexpand '(let ((a 1) (b 2)) a b c))
              '((lambda (a b) a b c) 1 2))
(assert-equal (totally-macroexpand '(or a b c))
              (totally-macroexpand
               '(let ((or-internal-argument a)) 
                  (if or-internal-argument or-internal-argument
                      (let ((or-internal-argument b))
                        (if or-internal-argument or-internal-argument
                            c))))))
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

;; There's a SRFI (SRFI 9?) that defines a way to use file operations
;; on strings (in this case, using open-input-string), but at least my
;; version of SCM doesn't support it.  But we need something like it
;; for testing.
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
          (( #\( ) (parse-list s))
          (( #\' ) (list 'quote (parse s)))
          (( #\" ) (parse-string-literal s))
          (( #\# ) (parse-hashy-thing s))
          (else (s 'unget) (parse-atom s))))))
(define (parse-list s)
  (let ((c (after-wsp s)))
    (if (parse-eof? c) (error "missing right paren")
        (case c
          (( #\) ) '())
          (( #\. ) (read-dotted-tail s))
          (else (let ((hd (begin (s 'unget) (parse s))))
                  (cons hd (parse-list s))))))))
(define (read-dotted-tail s)
  (let ((rv (parse s)))
    (if (eqv? #\) (after-wsp s)) rv (error "funky dotted list"))))
(define whitespace-chars "\n ")
(define (after-wsp s) 
  (let ((c (s))) (case c
                   ((#\space #\newline #\tab) (after-wsp s))
                   (( #\; ) (discard-comment s) (after-wsp s))
                   (else c))))
(define (discard-comment s) (if (eqv? (s) #\newline) #f (discard-comment s)))
;; XXX list->string
(define (parse-atom s) 
  (let ((atom (parse-atom-2 s)))
    (if (parsed-number? atom) (string->number (list->string atom))
        (string->symbol (list->string atom)))))
(define (char-numeric? char)            ; XXX standard
  (if (string-idx "0123456789" char) #t #f))
(define (parsed-number? lst)
  (cond ((null? lst) #f)
        ((char-numeric? (car lst)) (all-numeric? (cdr lst)))
        ((string-idx "+-" (car lst)) (and (not (null? (cdr lst))) 
                                          (all-numeric? (cdr lst))))
        (else #f)))
(define (all-numeric? lst)
  (or (null? lst) (and (char-numeric? (car lst)) (all-numeric? (cdr lst)))))
(define (parse-atom-2 s) 
  (let ((c (s))) 
    (if (parse-eof? c) '()
        (case c
          (( #\space #\newline #\tab #\; #\( #\) #\' #\" ) (s 'unget) '())
          (else (cons c (parse-atom-2 s)))))))
(define (parse-string-literal s) (list->string (parse-string-literal-2 s)))
(define (parse-string-literal-2 s)
  (let ((c (s))) 
    (case c                             ; XXX eof in string
      (( #\\ ) 
       (let ((next (s)))
         (let ((decoded
                (case next ((#\n) #\newline) ((#\t) #\tab) (else next))))
           (cons decoded (parse-string-literal-2 s)))))
       (( #\" ) 
        '())
       (else
        (cons c (parse-string-literal-2 s))))))
(define (parse-hashy-thing s)
  (let ((c (s))) 
    (if (parse-eof? c) (error "eof after #")
        (case c
          (( #\t ) #t)
          (( #\f ) #f)
          (( #\\ ) (parse-char-literal s))
          (else (error "Unimplemented #" c))))))
(define (parse-char-literal s)
  (let ((c (s))) (cond ((parse-eof? c) (error "eof in char literal"))
                       ((char-alphabetic? c) (s 'unget) (parse-named-char s))
                       (else c))))
(define (parse-named-char s)
  (let ((name (parse-atom-2 s)))
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
    (define (1+ x) (+ x 1))
    (define (1- x) (- x 1))
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
    (define (append a b)                ; standard
      (if (null? a) b (cons (car a) (append (cdr a) b))))

    ;; identical to standard caar, cdar, etc.
    (define (caar val) (car (car val)))
    (define (cdar val) (cdr (car val)))
    (define (cadr val) (car (cdr val)))
    (define (cddr val) (cdr (cdr val)))
    (define (caddr val) (car (cddr val)))
    (define (caadr val) (car (cadr val)))
    (define (cdadr val) (cdr (cadr val)))
    (define (not x) (if x #f #t))       ; standard

    ;; string manipulation
    (define (string-append-3 length s2 buf idx)
      (if (= idx (string-length buf)) buf
          (begin
            (string-set! buf idx (string-ref s2 (- idx length)))
            (string-append-3 length s2 buf (1+ idx)))))
    (define (string-append-2 s1 s2 buf idx)
      (if (= idx (string-length s1)) 
          (string-append-3 (string-length s1) s2 buf idx)
          (begin
            (string-set! buf idx (string-ref s1 idx))
            (string-append-2 s1 s2 buf (1+ idx)))))
    ;; XXX we could get rid of this if we weren't using it for creating error msgs
    ;; (and now, again, number->string)
    (define (string-append s1 s2)       ; standard
      (string-append-2 s1 s2 (make-string (+ (string-length s1) 
                                             (string-length s2)))
                       0))

    ;; chars
    (define (char-whitespace? c)
      (case c ((#\space #\newline #\tab) #t) (else #f)))

    ;; equality
    (define = eq?)
    ;; because chars are unboxed, char=? is eq?
    (define char=? eq?)
    ;; and all our numbers are unboxed too
    (define eqv? eq?)

    ;; equal? is a little trickier
    (define (equal? a b)
      (cond ((eq? a b) #t)
            ;; XXX need "and"
            ((string? a) (if (string? b) (string=? a b) #f))
            ;; XXX need "and"
            ((pair? a) (if (pair? b) (if (equal? (car a) (car b))
                                         (equal? (cdr a) (cdr b))
                                         #f)
                           #f))
            (else #f)))

    ;; string=? needs a loop
    (define (string=? a b)
      ;; XXX need "and"
      (if (= (string-length a) (string-length b)) (string=?-2 a b 0)
          #f))
    (define (string=?-2 a b idx)
      ;; XXX need "and"
      (or (= idx (string-length a))
          (if (char=? (string-ref a idx) (string-ref b idx))
              (string=?-2 a b (1+ idx))
              #f)))

    (define (null? x) (eq? x '()))
    (define (boolean? x) (if (eq? x #t) #t (eq? x #f)))

    (define (for-each proc list)   ; subset of standard: one list only
      (if (null? list) #f
          (begin
            (proc (car list))
            (for-each proc (cdr list)))))
    (define (map proc list)        ; subset of standard: one list only
      (if (null? list) '() (cons (proc (car list)) (map proc (cdr list)))))
    (define (string->list string)       ; standard
      (string->list-2 string (string-length string) '()))
    (define (string->list-2 string n rest)
      (if (= n 0) rest
          (string->list-2 string (- n 1)
                          (cons (string-ref string (- n 1)) rest))))
    (define (list->string lst)
      (list->string-2 (make-string (length lst)) lst 0))
    (define (list->string-2 buf lst idx)
      (if (null? lst) buf
          (begin (string-set! buf idx (car lst))
                 (list->string-2 buf (cdr lst) (1+ idx)))))
    (define (reverse lst) (reverse-plus '() lst))
    (define (reverse-plus tail lst) 
      (if (null? lst) tail (reverse-plus (cons (car lst) tail) (cdr lst))))
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
    (if (eof-object? expr) #t
        (begin (compile-toplevel expr)
               (read-compile-loop)))))

(compile-program read-compile-loop)
