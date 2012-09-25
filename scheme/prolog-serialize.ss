(library (prolog-serialize)

         (export pl-clause
                 pl-relation
                 pl-list
                 pl-conj
                 pl-disj
                 pl-cons
                 pl-ifte
                 pl-cond

                 scheme->pl

                 facts->asserts

                 suspend-clause
                 
                 display-pl)

         (import (rnrs)
                 (_srfi :1)
                 (util)
                 (printing))

         ;; <scheme-value> -> string basically

         ;; type pl-relation = string

         (define (scheme->pl x)
           (cond 
            ;;  [(and (list? x) (not (null? x)) (equal? 'call (car x)))
            ;;       (begin
            ;;         (pretty-print 'IS-CALL)
            ;;         (string-append "("
            ;;                        "G =.. [" (delimit ", " (map scheme->pl (cdr x))) "],"
            ;;                        "call(G)"
            ;;                        ")"))]

                  [(symbol? x) (symbol->string x)]
                 [(number? x) (number->string x)]
                 [(string? x) x]
                 [(list? x) (string-append (scheme->pl (car x)) 
                                           (if (null? (cdr x)) "" 
                                             (string-append "(" (delimit ", " (map scheme->pl (cdr x))) ")")))]
                 [else x]))

         (define (pl-clause lhs . bodies)
           (string-append lhs " :- " (apply pl-conj bodies) "."))

         (define pl-conj (lambda xs (string-append "(" (delimit ", " (map scheme->pl xs)) ")")))
         (define pl-disj (lambda xs (string-append "(" (delimit "; " (map scheme->pl xs)) ")")))

         (define (pl-list x . xs) 
           (if (null? x) "[]"
             (string-append ".(" (scheme->pl x) ", " (if (null? xs) "[]" (apply pl-list xs)) ")" )))

         (define (pl-relation f . args)
           (if (null? args) (scheme->pl f)
             (string-append (scheme->pl f) "(" (delimit ", " (map scheme->pl args)) ")" )))

         (define (pl-cons x y) (string-append "[" (scheme->pl x) "|" (scheme->pl y) "]"))

         (define (facts->asserts facts)
           (apply pl-conj (map (lambda (c) (string-append "assert((" c "))")) facts)))

         (define suspend-clause chop-last)

         (define (display-pl pl)
           (begin
             (for-each (lambda (p) (begin (display p)
                                          (newline)))
                       pl)))

         (define (pl-ifte i t e)
           (string-append "((" (scheme->pl i) ")->(" (scheme->pl t) ");(" (scheme->pl e) "))"))

         ;; Expression of Scheme conditional in Prolog
         
         (define (pl-cond . xs)
           (cond [(null? (cdr xs)) (car xs)]
                 [else (pl-ifte (car xs) 'true (pl-cond (cdr xs)))]))
         )
                 

