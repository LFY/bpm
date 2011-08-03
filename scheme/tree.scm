;;functions related to factor graphs
(library (tree)
         (export tree->expression 
                 attr-tree->expression
                 node-data->expression 
                 node-data->attr-expression

                 desugar 
                 gaussian->mean 
                 gaussian->variance 
                 ;replace-color 
                 replace-gaussian)
         (import (except (rnrs) string-hash string-ci-hash)
                 (program)
                 (util)
                 (printing)
                 (church readable-scheme)
                 (_srfi :1))
         (define GHOST-NODE 'GN)
         (define NODE 'N)
         (define DATA 'data)
        
         (define (attr-tree->expression tree) 
           (if (null? tree) (begin (display "done.\n") '())
             (begin
               (display "tree: ") (display tree) (display "\n")
               ;(display "first: ") (display (first tree)) (display "\n")
               ;(display "second: ") (display (second tree)) (display "\n")
               ;(display "rest: ") (display (rest tree)) (display "\n")
               (display "rest . rest: ") (display (rest (rest tree))) (display "\n")
               (pair (first tree) (pair (second tree) 
                                        (map attr-tree->expression 
                                                           (rest (rest tree))))))))

;;;basically put 'node at the front of every sub-expr and wrap in a lambda
         (define (tree->expression tree)
             (if (null? tree)
                 '()
                 (pair 'node (pair (node-data->attr-expression (first tree)) 
                                   (map tree->expression (rest tree))))))

         (define (node-data->expression lst)
           `(data (color (gaussian ,(first (second lst)) 25)) (size ,(first (third lst)))))

         ; generalized form of node-data->expression.
         ; wonder whether it's good to put down gaussians everywhere here...
         (define (node-data->attr-expression lst)
           (define (extract-number-combine-with-symbol idx-val)
             (let* ([val (first (second idx-val))]
                    [idx (first idx-val)]
                    [sym (string->symbol (format "A~d" idx))])
               `(,sym ,val)))
           (let* ([data-vals (cdr lst)])
             `(data ,@(map extract-number-combine-with-symbol (zip (iota (length data-vals)) data-vals)))))

         ;; Somehow these were defined in tree.church.
         ;(define node->data first)

         ;(define (node->color node)
         ;  (second (node->data node)))
         ;(define (node->children node)
         ;  (rest node))

         ;(define data->color second)
         ;(define data->size third)

        ;;used in scoring (computing the likelihood for a program that generates trees)
         ;;replace-color::program->program
         ;;
        ; (define (replace-color program)
        ;   (define (color? sexpr)
        ;     (tagged-list? sexpr 'color))
        ;   (define (return-parameters sexpr)
        ;     `(list ,(second sexpr) 15))
        ;   (define (replace-in-abstraction abstraction)
        ;     (make-named-abstraction (abstraction->name abstraction) 
        ;                             (sexp-search color? return-parameters (abstraction->pattern abstraction)) 
        ;                             (abstraction->vars abstraction)))
        ;   (let* ([converted-abstractions (map replace-in-abstraction (program->abstractions program))]
        ;          [converted-body (sexp-search color? return-parameters (program->body program))])
        ;     (make-program converted-abstractions converted-body)))

         ;; computing the score of continuous parameters of tree
        ; (define (compute-color-score tree tree-with-parameters)
        ;   (if (null? tree)
        ;     0
        ;     (+ (single-color-score (node->color tree) (node->color tree-with-parameters)) 
        ;        (apply + (map compute-color-score 
        ;                      (node->children tree) 
        ;                      (node->children tree-with-parameters))))))
        ; ;; subroutine: the score of a single tree color
        ; ;; basically uses gauss pdf
        ; (define (single-color-score x mean+variance)
        ;   (log (normal-pdf (first x) (first mean+variance) (second mean+variance))))

         (define (desugar program)
           (define (gaussian? sexpr)
             (tagged-list? sexpr 'gaussian))
           (define (uniform-choice? sexpr)
             (tagged-list? sexpr 'uniform-choice))
           (define (return-parameters sexpr)
             `(list 'gaussian-parameters ,(second sexpr) ,(third sexpr)))
           (define (uniform-draw-conversion sexpr)
             `((uniform-draw (list ,@(map thunkify (rest sexpr))))))
           (define tests+replacements (zip (list uniform-choice?) (list uniform-draw-conversion)))
           (define (apply-transforms sexpr)
             (fold (lambda (test+replacement expr)
                     (sexp-search (first test+replacement) (second test+replacement) expr))
                   sexpr
                   tests+replacements))
           (define (desugar-abstraction abstraction)
             (make-named-abstraction (abstraction->name abstraction) (apply-transforms (abstraction->pattern abstraction)) (abstraction->vars abstraction)))
           (let* ([converted-abstractions (map  desugar-abstraction (program->abstractions program))]
                  [converted-body (apply-transforms (program->body program))])
             (make-program converted-abstractions converted-body)))

         (define (replace-gaussian program)
           (define (gaussian? sexpr)
             (tagged-list? sexpr 'gaussian))
           (define (return-parameters sexpr)
             `(list 'gaussian-parameters ,(second sexpr) ,(third sexpr)))
           (define (replace-in-abstraction abstraction)
             (make-named-abstraction (abstraction->name abstraction) (sexp-search gaussian? return-parameters (abstraction->pattern abstraction)) (abstraction->vars abstraction)))
           (let* ([converted-abstractions (map replace-in-abstraction (program->abstractions program))]
                  [converted-body (sexp-search gaussian? return-parameters (program->body program))])
             (make-program converted-abstractions converted-body)))

         (define gaussian->mean second)
         (define gaussian->variance third))
