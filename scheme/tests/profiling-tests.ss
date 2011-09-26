(import (profiling)
        (printing)
        (_srfi :1))

(define-timed (select-k-subsets n l)
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

(time (select-k-subsets 10 (iota 24)))
(time (select-k-subsets 10 (iota 24)))
(time (select-k-subsets 10 (iota 24)))
(time (select-k-subsets 10 (iota 24)))
(time (select-k-subsets 10 (iota 24)))

(print (current-profiling-stats))
