(define (f1)
  (choose `(node ,(f1)) 2))

(define (f2 x)
  `(node (node ,x)
         (node ,x)))

(f2 (f1))
          
