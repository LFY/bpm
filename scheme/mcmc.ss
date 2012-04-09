(library (mcmc)
         (export run-mcmc
                 run-local-search
                 num-iter-stop
                 run-multiple-try-local-search)
         (import (rnrs)
                 (printing)
                 (_srfi :1)
                 (util)
                 (delimcc-simple-ikarus))

         (define (num-iter-stop iter-fx n)
           (define counter 0)
           (define (fval curr next score-ratio correction accept)
             (let* ([curr-state (list counter curr next score-ratio correction accept)])
               (begin
                 (if (< counter n)
                   (iter-fx curr-state)
                   (begin
                     (iter-fx curr-state)
                     (shift k 'STOP)))
                 (set! counter (+ 1 counter)))))
           fval)

         (define (run-mcmc
                   init-state
                   prop
                   score
                   iter)

           (define (accept? log-prob)
             (if (> log-prob 0.0) #t
               (let ([smp (uniform-sample 0 1)])
                 (if (= 0.0 smp) #f
                   (let ([log-smp (log smp)])
                     (< log-smp log-prob))))))

           (define (loop curr prop score iter)
             (let* ([correction+next (prop curr)]
                    [next (cadr correction+next)]
                    [correction (car correction+next)]
                    [score-ratio (score next curr)]
                    [score-ratio+correction (+ correction score-ratio)]
                    [accept (accept? score-ratio+correction)]
                    [void (iter curr next score-ratio correction accept)])
               (if accept
                 (loop next prop score iter)
                 (loop curr prop score iter))))

           (reset
             (list "MCMC run result:"
                   (loop init-state prop score iter))))

         (define (run-local-search
                   init-state
                   prop
                   score
                   iter)

           (define best init-state)
           (define best-score -900000)

           (define (accept? log-prob)
             (if (> log-prob 0.0) #t
               (let ([smp (uniform-sample 0 1)])
                 (if (= 0.0 smp) #f
                   (let ([log-smp (log smp)])
                     (< log-smp log-prob))))))

           (define (loop curr prop score iter)
             (let* ([correction+next (prop curr)]
                    [next (cadr correction+next)]
                    [correction (car correction+next)]
                    [score-ratio (score next curr)]
                    [score-ratio+correction (+ 0.0 score-ratio)]
                    [accept (accept? score-ratio+correction)]
                    [void (iter curr next score-ratio correction accept)])
               (if accept
                 (loop next prop score iter)
                 (loop curr prop score iter))))

           (reset
             (list "MCMC run result:"
                   (loop init-state prop score iter))))


         (define (maximum f xs)
           (define (loop v c xs)
             (cond [(null? xs) c]
                   [else (let* ([next (f (car xs))])
                           (if (> next v) 
                             (loop next (car xs) (cdr xs))
                             (loop v c (cdr xs))))]))
           (loop (f (car xs)) (car xs) (cdr xs)))


         (define (run-multiple-try-local-search
                   fan-out
                   init-state
                   init-score
                   prop
                   iter)

           (define (accept? log-prob)
             (if (> log-prob 0.0) #t
               (let ([smp (uniform-sample 0 1)])
                 (if (= 0.0 smp) #f
                   (let ([log-smp (log smp)])
                     (< log-smp log-prob))))))

           (define (loop curr-state curr-score prop iter)
             (let* ([correction+score+next-states (map (lambda (i) (prop curr-state)) (iota fan-out))]
                    [best (maximum cadr correction+score+next-states)]
                    [best-score (cadr best)]
                    [best-state (caddr best)]
                    [score-ratio (- best-score curr-score)]
                    [accept (accept? score-ratio)]
                    [void (iter curr-state best-state score-ratio (car best) accept)])
               (if accept
                 (loop best-state best-score prop iter)
                 (loop curr-state curr-score prop iter)
                 )))

                                                       
           ;; (define (loop curr prop score iter)
           ;;   (let* ([correction+next-states (map (lambda (i) (prop curr)) (iota fan-out))]
           ;;          [correction+next (car (sort (lambda (x y) (> (score (cadr x) curr) (score (cadr y) curr)))
           ;;                                      correction+next-states))]
           ;;          [next (cadr correction+next)]
           ;;          [correction (car correction+next)]
           ;;          [score-ratio (score next curr)]
           ;;          [score-ratio+correction (+ 0.0 score-ratio)]
           ;;          [accept (accept? score-ratio+correction)]
           ;;          [void (iter curr next score-ratio correction accept)])
           ;;     (if accept
           ;;       (loop next prop score iter)
           ;;       (loop curr prop score iter))))

           (reset
             (list "MCMC run result:"
                   (loop init-state init-score prop iter))))
         )
