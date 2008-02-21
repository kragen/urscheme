;;; Test "write" procedure.

(define (wnl x) (write x) (newline))
(for-each wnl '("hello"
                "Let's go to \"C:\\AGENDA\" for a good semistructured time!"
                ()
                ("hi")
                foo
                (foo)
                (foo bar)
                (foo bar baz)
                (foo bar . baz)
                3
                (1 2 3 5)
                (#t #f)
                (#\x #\y #\newline #\space #\\)))