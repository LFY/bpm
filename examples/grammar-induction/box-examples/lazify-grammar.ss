(import (grammars)
        (printing)
        (_srfi :1)
        (util)
        (program)
        (delimcc-simple-ikarus))

;; New grammar preprocessing function: lazify-nts, which uses the Scheme
;; procedure representation of nonterminals to return a function (not a tree)
;; that, when called, uses shift to capture the current context and replace it
;; with a local probability and the subtree associated with the nonterminal
;; attached to the provided context.

(define (lazify-nts grammar+params)

  (define (grammar->params grammar+params)
    (cadddr grammar+params))

  (define (my-grammar->nts grammar+params)
    (append (program->abstractions grammar+params)
            (list `(abstraction TopLevel () ,(caddr (program->body grammar+params))))))

  (define nts-with-params
    (map (lambda (nt params)
           (let* ([choices (cond [(eq? 'choose (car (abstraction->pattern nt))) 
                                  (cdr (abstraction->pattern nt))]
                                 [else (list (abstraction->pattern nt))])])
             `(abstraction ,(abstraction->name nt) ()
                           (lambda () ;; new: defers computation so that we may work with nested calls to shift-using functions
                             (shift k 
                                    (list ,@(map 
                                              (lambda (param thunk) 
                                                `(list 
                                                   ,(exp param) ;; local probability of choice
                                                   (k ,thunk) ;; k : whatever the rest of the model is
                                                       )) params 
                                              choices)))))))
         (my-grammar->nts grammar+params) (grammar->params grammar+params)))

  `(program ,nts-with-params (TopLevel)))

(define test-grammar '(program
                        ((abstraction F33 ()
                                      (choose (elem "gray" (tr "forward" (F33)))
                                              (elem "gray" (tr "forward" (F28)))
                                              (elem "gray" (tr "forward" (F18)))))
                         (abstraction F30 ()
                                      (choose
                                        (elem "root" (tr "left" (F33)) (tr "right" (F25)))
                                        (elem "root" (tr "left" (F10)) (tr "right" (F33)))
                                        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
                         (abstraction F28 () (elem "blue" (tr "forward" (F21))))
                         (abstraction F27 () (elem "red" (tr "forward" (F0))))
                         (abstraction F21 () (elem "blue"))
                         (abstraction F23 () (elem "blue" (tr "forward" (F28))))
                         (abstraction F24 () (elem "gray" (tr "forward" (F23))))
                         (abstraction F25 () (elem "gray" (tr "forward" (F24))))
                         (abstraction F0 () (elem "red"))
                         (abstraction F1 () (elem "gray" (tr "forward" (F0))))
                         (abstraction F2 () (elem "gray" (tr "forward" (F1))))
                         (abstraction F4 () (elem "gray" (tr "forward" (F21))))
                         (abstraction F5 () (elem "gray" (tr "forward" (F4))))
                         (abstraction F9 () (elem "gray" (tr "forward" (F27))))
                         (abstraction F10 () (elem "gray" (tr "forward" (F9))))
                         (abstraction F18 () (elem "red" (tr "forward" (F27)))))
                        (lambda () (choose (F30)))
                        ((-1.0986122886681098 -1.0986122886681098
                                              -1.0986122886681098)
                         (-1.0986122886681098 -1.0986122886681098
                                              -1.0986122886681098)
                         (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
                         (0.0) (0.0) (0.0) (0.0) (0.0) (0.0))
                        (stats (posterior -128.0684255882441)
                               (likelihood+weight -7.454719949364001 1.0)
                               (prior+weight -120.6137056388801 1.0) (desc-length 122)
                               (dirichlet-prior 1.3862943611198912))
                        ()))

(define test-grammar2
  `(program
     ((abstraction F1 ()
                   (elem "root"
                         (tr "left" (F2))
                         (tr "middle" (F2))
                         (tr "right" (F2))))
      (abstraction F2 ()
                   (choose
                     (elem "blue")
                     (elem "red"))))
     (lambda () (choose (F1)))
     ((0.0)
      ,(map log '(0.5 0.5))
      (0.0))))
      

;; Basically map but for trees

(define (tree-walk f expr)
  (begin 
    (cond [(null? expr) (f expr)]
          [(list? expr) (let* ([new-expr (f expr)])
                          (cond [(list? new-expr)
                                 (cons (tree-walk f (car new-expr))
                                       (tree-walk f (cdr new-expr)))]
                                [else new-expr]))]
          [else (f expr)]
          )))

;; Utility functions

;; Detects if this tree has a probability attached (used in compress)

(define (probtree? tree)
  (number? (car tree)))

;; Detects if the model is done and there are no further thunks

(define (partial? prob-tree)
  (reset 
    (begin (tree-walk (lambda (t) (cond [(procedure? t) (shift k #t)]
                                        [else t]))
                      prob-tree)
           #f)))

;; expand: traverses a partial tree down to the procedures, which were set up
;; before to capture the current context and replace it with 
;; (list (list prob1 v1) (list prob2 v2) ...)

(define (expand prob-partial-trees)
  (map
    (lambda (prob-partial-tree)
      (let* ([curr-prob (car prob-partial-tree)]
             [partial-tree (cadr prob-partial-tree)]
             [next-trees-with-prob
               ;; what happens here:
               ;; we evaluate procedure that was created by the original nonterminal,
               ;; executing (shift k (list (list 1.0 (k model....))))
               ;; then we encounter (reset (tree-walk ...
              
               ;; and next-trees-with-prob becomes
               ;; (list (list 1.0 (tree-walk ... model ...)))
               ;; (where ... is the surrounding part, partial-tree)

               (reset (tree-walk
                        (lambda (t) (cond [(procedure? t) (t)]
                                          [else t]))
                        partial-tree))]
             )
        (list curr-prob next-trees-with-prob)))
    (filter partial? prob-partial-trees)))

;; compress: takes output of expand, and multiplies out probabilties, resulting
;; in a flat list of (prob, partial-model) 's that can be used as input to
;; expand again

(define (compress layered-partial-trees)
  (define (no-inner-prob-trees? trees)
    (reset
      (begin
        (map (lambda (prob-tree)
               (let* ([tree (cadr prob-tree)])
                 (cond [(and (list? tree) (list? (car tree)) (probtree? (car tree)))
                        (shift k #f)]
                       [else tree])))
             trees)
        #t)))

  (define (loop layered-partial-trees)
    (cond [(no-inner-prob-trees? layered-partial-trees) layered-partial-trees]
          [else (loop (concatenate (map
                                     (lambda (prob-tree1)
                                       (let* ([prob (car prob-tree1)]
                                              [trees (cadr prob-tree1)])
                                         (cond [(and (list? trees) (list? (car trees)) (probtree? (car trees)))
                                                (map (lambda (prob-tree2)
                                                       (let* ([next-prob (car prob-tree2)]
                                                              [next-tree (cadr prob-tree2)])
                                                         (list (* next-prob prob) next-tree))) trees)]
                                               [else (list (list prob trees))])))
                                     layered-partial-trees)))]))
  (loop layered-partial-trees))

;; Why is compress an iterative function? Shouldn't we just need to compress
;; the list of probabilities one level? This has to do with how multiple calls
;; to a function that uses shift, under one reset, work. What ends up happening
;; is that all possible delimited continuations are produced.

;; Example: for f1 one k (delimited continuation) includes the
;; previously-finished (list 'H ...) context and re-introduces a (list 'H ...),
;; causing further nesting

;; (define (f1)
;;   (shift k (list 'H 
;;                  (k 1)
;;                  (k 2))))
;; 
;; thus, for every extra call of (f1) we end up with another level of nesting
;; of 'H

;; (pretty-print (reset (list (f1) )))
;; (pretty-print (reset (list (f1) (f1)  )))
;; (pretty-print (reset (list (f1) (f1) (f1) )))
;; (pretty-print (reset (list (f1) (f1) (f1) (f1))))
;; 
;; produces:
;;
;; (H (1) (2))
;; (H (H (1 1) (1 2)) (H (2 1) (2 2)))
;; (H (H (H (1 1 1) (1 1 2)) (H (1 2 1) (1 2 2)))
;;   (H (H (2 1 1) (2 1 2)) (H (2 2 1) (2 2 2))))
;; (H
;;   (H (H (H (1 1 1 1) (1 1 1 2)) (H (1 1 2 1) (1 1 2 2)))
;;     (H (H (1 2 1 1) (1 2 1 2)) (H (1 2 2 1) (1 2 2 2))))
;;   (H (H (H (2 1 1 1) (2 1 1 2)) (H (2 1 2 1) (2 1 2 2)))
;;     (H (H (2 2 1 1) (2 2 1 2)) (H (2 2 2 1) (2 2 2 2)))))

;; explore: a single breadth-first expansion.
;; think of it as performing the parallel rewrite step of the L-system once,
;; tracking probabilities and producing another portion of the model.

(define (explore x) (compress (expand x)))

;; Example usage

(define lazified-grammar (lazify-nts test-grammar))

(pretty-print (program->sexpr lazified-grammar))

(define thunk-tree (eval (program->sexpr lazified-grammar) 
                         (environment '(rnrs) 
                                      '(util) 
                                      '(program) 
                                      '(grammar-derivations-spread) 
                                      '(delimcc-simple-ikarus) 
                                      '(_srfi :1))))

(define initial-distr (reset (thunk-tree)))
(pretty-print initial-distr)
(pretty-print (explore initial-distr))
(pretty-print (explore (explore initial-distr)))
(pretty-print (explore (explore (explore initial-distr))))


