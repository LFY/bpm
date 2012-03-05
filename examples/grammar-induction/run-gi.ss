(import (mcmc)
        (grammar-proposals)
        (grammar-likelihood)
        (printing)
        (_srfi :1)
        (scene-graphs))

(define best-state '())

(define (print-best-score)
  (define best-score '())
  (define iter 0)
  (lambda (state)
    (let* ([accept? (list-ref state 5)]
           [next-state (list-ref state 2)]
           [next-state-score (grammar->grammar+posterior test-data next-state 1.0 1.1 0.8)]
           [next-state (car next-state-score)]
           [next-score (cadr next-state-score)]
           )
      (if (and accept? (or (null? best-score) (> next-score best-score)))
        (begin
          (set! best-score next-score)
          (set! best-state next-state)
          (pretty-print `(define ,(string->symbol (string-append "grammar-iter" (number->string iter)))
                           (quote ,next-state)))
          (set! iter (+ 1 iter)))
        (begin
          (set! iter (+ 1 iter)))))))

(define (opt-select xs idx val)
  (if (<= (length xs) idx) val (list-ref xs idx)))

(define argv (command-line))
(define file-to-load (list-ref argv 1))
(define fan-out (string->number (opt-select argv 2 "10")))
(define num-iter (string->number (opt-select argv 3 "100")))

(load file-to-load)

(run-multiple-try-local-search fan-out
  (init-grammar test-data)
  (split-merge-proposal test-data)
  (lambda (next curr) (- (grammar->posterior test-data next 1.0 1.1 0.8)
                         (grammar->posterior test-data curr 1.0 1.1 0.8)))
  (num-iter-stop (print-best-score) num-iter))

(define best-gr (car (grammar->grammar+posterior test-data best-state 1.0 1.1 0.8)))

(pretty-print `(define grammar (quote ,best-gr)))
(pretty-print `(define transforms (quote ,transforms)))

