(+ (- 1 2) (* 3 (/ 4 (remainder 5 6))))
(eq? (+ 3.0 2) (/ 6 2))
(eq? #t 1)
foo
(add 1 2)
(define foo (add a b)
    (+ a b))
(define add
    (lambda (a b)
        (+ a b)))
(define deriv (lambda (expr var)
    (if expr var 1)))
