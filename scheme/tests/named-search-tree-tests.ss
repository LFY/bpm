(import (named-search-trees)
        (printing)
        (_srfi :1)
        (util))

;; Without syntax sugar

(define (program1)
  (reify-choice-context 'program1
                        (lambda () (f2 (f1 (f5))
                                       (f1 (f5))))))

(define (f5) (reify-choice-context 'f5 (lambda () `(p ,(make-choice-NT (lambda () 1) (lambda () 2))))))
(define (f1 x) (reify-choice-context `(f1 ,x) (lambda () `(node ,(make-choice-NT (lambda () x) (lambda () 'b))))))

'((define-NT Start
             (img (node (C 1 (f1 (p (C 1 f5)))))
                  (node (C 1 (f1 (p (C 1 f5)))))))
  ((define-NT (C 1 f5) (choose 1 2))
   (define-NT (C 1 (f1 (p (C 1 f5))))
              (choose (p (C 1 f5)) b))))

;; With syntax sugar:

(define (f2 x y) `(img ,x ,y))

(define-nondet (f1-nondet x) 
               `(node ,(nondet-choice x 'b)))

(define-nondet (f5-nondet)
               `(p ,(nondet-choice 1 2)))

(define-nondet (program2)
                    (f2 (f1-nondet (f5-nondet))
                        (f1-nondet (f5-nondet))))

;; Recursion

(define-nondet (f3-nondet)
               `(node ,(nondet-choice (f3-nondet) (f5-nondet))))

(define-nondet (program3)
                    (f3-nondet))

;; We can also handle functions as arguments
;; (not functions being returned; we would need to obtain the closure)

(define-nondet (h1 x y)
               `(node ,(nondet-choice (x) (y))))
(define-nondet (h2)
               (nondet-choice 1 2))
(define-nondet (h3)
               (nondet-choice 3 4))

(define-nondet (program4)
                    `(node ,(h1 h2 h3)
                           ,(h1 h3 h2)))

;; Mutual recursion

(define-nondet (m1) `(node ,(nondet-choice (m2) 1)))
(define-nondet (m2) `(node ,(nondet-choice (m1) 2)))
(define-nondet (program5) (m1))

;; Multiple choices per abstraction

(define-nondet (f-mult-choice) `(node ,(nondet-choice 0 1) ,(nondet-choice 2 3)))
(define-nondet (program6) `(p ,(f-mult-choice)
                                   ,(f-mult-choice)))

;; Putting everything together; the corresponding SCFG that only puts
;; nonterminals (other than axiom) at choice points is visibly different from
;; the functional form

(define-nondet (F1) `(node ,(nondet-choice 1 2) ,(nondet-choice (F1) (F2))))
(define-nondet (F2) `(div ,(nondet-choice 0 1)))

(define-nondet (program7) (F1))

(define-nondet (NT1) (nondet-choice 0 1))
(define-nondet (program8) `(node ,(NT1) ,(NT1)))

(print "Simple program")
(pretty-print (nondet-program->named-search-tree program1))
(newline)

(print "Simple program (From sugared version)")
(pretty-print (nondet-program->named-search-tree program2))
(newline)

(print "Recursion")
(pretty-print (nondet-program->named-search-tree program3))
(newline)

(print "Higher-order")
(pretty-print (nondet-program->named-search-tree program4))
(newline)

(print "Mutual recursion")
(pretty-print (nondet-program->named-search-tree program5))
(newline)

(print "Multiple choices in one abstraction")
(pretty-print (nondet-program->named-search-tree program6))
(newline)

(print "Combination of recursion + multiple choices")
(pretty-print (nondet-program->named-search-tree program7))
(newline)

(print "Choices w/ same name")
(pretty-print (nondet-program->named-search-tree program8))
(newline)

(define-nondet (f9) (nondet-choice 0 1 2))
(define-nondet (program9)
               (map (lambda (x) `(node ,(nondet-choice x (f9)))) (iota 5)))

(print "Map")
(pretty-print (nondet-program->named-search-tree program9))
(newline)

(define-nondet (f100) `(node 0 0))
(define-nondet (f101) `(node 1 1))

(define (f10_0 x y) `(node ,x ,y))

(define-nondet (f10) (nondet-choice (f10_0 0 0) (f10_0 1 1)))

(print "Choice splitting")
(pretty-print (nondet-program->named-search-tree f10))
