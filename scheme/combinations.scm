(library (combinations)
         (export cartesian-product
                 select-k
                 select-k-comm)
         (import (rnrs)
                 (_srfi :1)
                 (util))

         ; Cartesian product
         (define (cartesian-product . xs)
           (define (loop acc xs)
             (if (null? xs)
               (list (reverse acc))
               (concatenate 
                 (map (lambda (x)
                        (loop
                          (cons x acc)
                          (rest xs))) (first xs)))))
           (loop '() xs))

         ; all ways to select k elements from a list without repetition; k-permutations
         (define (select-k k xs)
           (define (loop acc k xs)
             (if (eq? 0 k)
               (list acc)
               (concatenate 
                 (map (lambda (x)
                        (loop 
                          (cons x acc) 
                          (- k 1) 
                          (delete x xs))) xs))))
           (loop '() k xs))

         ; all ways to select k elements from a list without repetition + having commutative arguments
         (define (select-k-comm k xs)
           (define (loop acc k xs)
             (if (eq? 0 k)
               (list acc)
               (concatenate 
                 (map (lambda (x)
                        (loop 
                          (cons x acc) 
                          (- k 1) 
                          (rest (member x xs)))) xs))))
           (loop '() k xs))

         ; Bad old version of select-k
         ;(define (select-k k xs)
         ;  (define (loop acc used rem)
         ;    (if (null? rem)
         ;      (list (reverse acc))
         ;      (let* ([x (first rem)]
         ;             [rs (rest rem)])
         ;        (concatenate 
         ;          (map 
         ;            (lambda (e)
         ;              (loop (cons e acc) (cons e used) rs))
         ;            (filter (lambda (y)
         ;                      (equal? #f (member y used))) x))))
         ;      ))
         ;  (loop '() '() (map (lambda (i) xs) (iota k))))

         )
