(library (combinations)
         (export cartesian-product
                 select-k
                 select-k-comm
                 select-k-subsets
                 select-k-subsets-gen)
         (import (rnrs)
                 (_srfi :1)
                 (util)
                 (generators))

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


         ;; Much faster version of select-k-comm by Oleg Kiselyov
         (define (select-k-subsets n l)
           (let loop ((l l) (ln (length l)) (n n) (prev-els '()) (accum '()))
             (cond
               ((<= n 0) (cons prev-els accum))
               ((< ln n) accum)
               ((= ln n) (cons (append l prev-els) accum))
               ((= ln (+ 1 n)) 
                (let fold ((l l) (seen prev-els) (accum accum))
                  (if (null? l) accum
                    (fold (cdr l) (cons (car l) seen)
                          (cons
                            (append (cdr l) seen)
                            accum)))))
               ((= n 1)
                (let fold ((l l) (accum accum))
                  (if (null? l) accum
                    (fold (cdr l) (cons (cons (car l) prev-els) accum)))))
               (else
                 (loop (cdr l) (- ln 1) n prev-els
                       ; new accum
                       (loop (cdr l) (- ln 1) (- n 1) (cons (car l) prev-els) accum))))))

         ;; returns a thunk which, when evaluated, returns another value from
         ;; the stream + another thunk. when 'END is returned, it's over
        

         ;; generator version...quite slow

         (define (select-k-subsets-gen n l)
           (let loop ((l l) (ln (length l)) (n n) (prev-els '()))
             (cond
               ;; ((<= n 0) (yield-> (cons prev-els accum)))
               ((<= n 0) (yield-> prev-els))
               ((< ln n) (yield-> '()))
               ((= ln n) (yield-> (append l prev-els)))
               ((= ln (+ 1 n)) 
                (let fold ((l l) 
                           (seen prev-els) 
                           )
                  (if (null? l) (begin 'End)
                    (let* ([next-subset (append (cdr l) seen)])
                      (begin (yield-> next-subset)
                             (fold (cdr l)
                                   (cons (car l) seen)))))))
               ((= n 1)
                (let fold ((l l))
                  (if (null? l) (begin 'End)
                    (let* ([next-subset (cons (car l) prev-els)])
                      (begin (yield-> next-subset)
                             (fold (cdr l)))))))
               (else
                 (begin
                   (forin-> yield-> (lambda () (loop (cdr l) (- ln 1) (- n 1) (cons (car l) prev-els))))
                   (forin-> yield-> (lambda () (loop (cdr l) (- ln 1) n prev-els)))
                   )
                 ))))

         ;; TODO: lazy stream version 
         


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
