(import (_srfi :1)
        (util)
        (printing))

(define (sexp-walk f expr)
 (cond [(null? expr) expr]
       [(list? expr) (let* ([new-expr (f expr)])
                       (cons (sexp-walk f (car new-expr))
                             (sexp-walk f (cdr new-expr))))]
       [else expr]))

(define choices '((Choice15) (Choice14) (Choice16)))

(define expr '(choose (node (Choice15) (Choice14)) (div (Choice16))))

(pretty-print (sexp-walk (lambda (t) (cond [(contains? t choices) (list (car t) 0)]
                                           [else t]))
                         expr))
