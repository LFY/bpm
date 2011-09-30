(import (printing)
        (sxml2scfg))

         (define (elt? sym) 
           (and (symbol? sym)
                (eq? 'elem sym)))

         (define elt-pred (lambda (e) (and (not (null? e)) 
                                           (list? e) 
                                           (not (null? (cdr e))) 
                                           (elt? (car e)))))

(define test-data 
  (list 
    '(elem "e1"
         (tr "trans1" (elem "e1"))
         (tr "trans2" (elem "e1"))
         (tr "trans1" (elem "e1")))
    ))

(pretty-print test-data)
(pretty-print (sxmls->initial-program elt-pred test-data))
