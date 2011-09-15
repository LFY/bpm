(import (_srfi :1)
        (util)
        (printing)
        (scfg))

(define test-scfg '((define (Start) (node (Choice15) (Choice14)))
                    ((define (Choice14)
                       (choose (node (Choice15) (Choice14)) (div (Choice16))))
                     (define (Choice15) (choose 1 2))
                     (define (Choice16) (choose 0 1)))))

(print (scfg->axiom test-scfg))
(print (scfg->productions test-scfg))
(print (scfg->nonterminal-names test-scfg))

(define nt-names (scfg->nonterminal-names test-scfg))

(print (map (lambda (p) (list (prod->name p) (prod->successors test-scfg p))) (scfg->productions test-scfg)))

(print (map (lambda (p) (list (prod->name p) (prod->choices p))) (scfg->productions test-scfg)))
(print (map (lambda (p) (list (prod->name p) (prod->choices p))) (scfg->productions test-scfg)))

(print (map (lambda (p) (list (prod->name p) (rename-successors test-scfg p (lambda (v) (car v))))) (scfg->productions test-scfg)))

(pretty-print (prefix-scfg "Grammar12_" test-scfg))
(pretty-print (scfg->start-name (prefix-scfg "Grammar12_" test-scfg)))
