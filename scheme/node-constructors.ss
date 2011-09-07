(library (node-constructors)
         (export node
                 data
                 a0
                 a1
                 a2
                 a3

                 p

                 NULL HTML BODY DIV A UL LI H1 H2 P
                 name left top width height
                 x y
                 )
         (import (rnrs)
                 (_srfi :1))

         (define (node attrs . children) (append (list 'node attrs) children))
         (define (data . attrs) (append (list 'data) attrs))

         (define (a0 x) (list 'a0 x))
         (define (a1 x) (list 'a1 x))
         (define (a2 x) (list 'a2 x))
         (define (a3 x) (list 'a3 x))

         (define (p . xs) (append (list 'p) xs))

         ;; HTML stuff
         (define NULL 'NULL)
         (define HTML 'HTML)
         (define BODY 'BODY)
         (define DIV 'DIV)
         (define A 'A)
         (define UL 'UL)
         (define LI 'LI)
         (define H1 'H1)
         (define H2 'H2)
         (define P 'P)

         (define (left x) `(left ,x))
         (define (top x) `(top ,x))
         (define (width x) `(width ,x))
         (define (height x) `(height ,x))
         (define (name x) `(name ,x))
         (define (x z) `(x ,z))
         (define (y z) `(y ,z))


         )
