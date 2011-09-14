(import (printing)
        (sxml2scfg))

(define (elt? sym) 
  (and (symbol? sym)
       (equal? "e" (substring (symbol->string sym) 0 1))))

(define elt-pred (lambda (e) (and (not (null? e)) 
                                  (list? e) 
                                  (not (null? (cdr e))) 
                                  (elt? (car e)))))


(define test-data 
  (list 
    '(e1 (m11 (e2))
         (m12 (e1))
         (m11 (e3)))
    '(e1 (m11 (e2 
                (m111 (e3))
                (m112 (e4))))
         (m12 (e1)))
    '(e4 (m23 (e4))
         (m24 (e5 (m34 (e6))
                  (m33 (e8 (m11 (e0))
                           (m12 (e1))))))
         (m23 (e3)))
    ))

(pretty-print test-data)
(pretty-print (sxmls->initial-program elt-pred test-data))
