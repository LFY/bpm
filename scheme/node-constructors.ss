(library (node-constructors)
         (export node
                 data
                 a0
                 a1
                 a2
                 a3
                 )
         (import (rnrs)
                 (_srfi :1))

         (define (node attrs . children) (append (list 'node attrs) children))
         (define (data attr1 . attrs) (append (list 'data attr1) attrs))

         (define (a0 x) (list 'a0 x))
         (define (a1 x) (list 'a1 x))
         (define (a2 x) (list 'a2 x))
         (define (a3 x) (list 'a3 x))

         )
