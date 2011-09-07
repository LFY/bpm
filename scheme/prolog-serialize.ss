(library (prolog-serialize)

         (export pl-clause
                 pl-relation
                 pl-list
                 pl-conj
                 pl-disj

                 scheme->pl
                 
                 )

         (import (rnrs)
                 (_srfi :1)
                 (util)
                 (printing))

         ;; <scheme-value> -> string basically

         ;; type pl-relation = string

         (define (scheme->pl x)
           (cond [(symbol? x) (symbol->string x)]
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

         )
                 

