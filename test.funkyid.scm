;;; Test of funky identifiers.

(define !$%&*+-./:<=>?@^_~ ": yes")
(display (symbol->string '!$%&*+-./:<=>?@^_~))
(display !$%&*+-./:<=>?@^_~) (newline)
