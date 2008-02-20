;;; First interpreter from Steele and Sussman's "Art of the Interpreter"
;; (AIM-453) translated into modern Scheme.

;;; Figure 1, p.8 (ninth page)

(define (driver)
  (driver-loop <the-primitive-procedures> (print "lithp ith lithtening")))

(define (driver-loop procedures hunoz)
  (driver-loop-1 procedures (read)))

(define (driver-loop-1 procedures form)
  (cond ((symbol? form)
         (driver-loop procedures (print (eval form '() procedures))))
        ((eq? (car form) 'define)
         (driver-loop (bind (list (caadr form))
                            (list (list (cdadr form) (caddr form)))
                            procedures)
                      (print (caadr form))))
        (else (driver-loop procedures (print (eval form '() procedures))))))

;;; Figure 2, p.9 (tenth page)

(define (eval exp env procedures)
  (cond ((symbol? exp)
         (cond ((eq? exp 'nil) 'nil)
               ((eq? exp 't) 't)
               ((number? exp) exp)
               (else (value exp env))))
        ((eq? (car exp) 'quote)
         (cadr exp))
        ((eq? (car exp) 'cond)
         (evcond (cdr exp) env procedures))
        (else (apply (value (car exp) procedures)
                     (evlis (cdr exp) env procedures)
                     procedures))))

(define (apply fun args procedures)
  (if (primop? fun) (primop-apply fun args)
      (eval (cadr fun)
            (bind (car fun) args '())
            procedures)))

(define (evcond clauses env procedures)
  (cond ((null? clauses) (error "don't know what to do!"))
        ((eval (caar clauses) env procedures)
         (eval (cadar clauses) env procedures))
        (else (evcond (cdr clauses) env procedures))))

(define (evlis arglist env procedures)
  (cond ((null? arglist) '())
        (else (cons (eval (car arglist) env procedures)
                    (evlis (cdr arglist) env procedures)))))

;;; Figure 3, p.11 (twelfth page)

(define (bind vars args env)
  (if (= (length vars) (length args))
      (cons (cons vars args) env)
      (error "argument mismatch")))

(define (value name env)
  (value1 name (lookup name env)))

(define (value1 name slot)
  (if (eq? slot '&unbound) (error "unbound slot")
      (car slot)))

(define (lookup name env)
  (if (null? env) '&unbound
      (lookup1 name (caar env) (cdar env) env)))

(define (lookup1 name vars vals env)
  (cond ((null? vars) (lookup name (cdr env)))
        ((eq? name (car vars)) vals)
        (else (lookup1 name (cdr vars) (cdr vals) env))))

;;; Supplemental material to make it executable in Ur-Scheme.

;; We don't have "apply", so...
(define (onearg fn) (lambda (args) (fn (car args))))
(define (twoarg fn) (lambda (args) (fn (car args) (cadr args))))

(define <the-primitive-procedures>
  (list
   (cons '(cons cdr car eq? 
                symbol? number? null?)
         (list (twoarg cons) (onearg cdr) (onearg car) (twoarg eq?) 
               (onearg symbol?) (onearg number?) (onearg null?)))))

(define primop? procedure?)
(define (primop-apply fn args) (fn args))

(define (print foo) (display foo) (newline))


(driver)
