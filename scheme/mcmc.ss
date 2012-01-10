(library (mcmc)
         (export run-mcmc
                 num-iter-stop)
         (import (rnrs)
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
             (let* ([next+correction (prop curr)]
                    [next (car next+correction)]
                    [correction (cadr next+correction)]
                    [score-ratio (score next curr)]
                    [score-ratio+correction (+ correction score-ratio)]
                    [accept (accept? score-ratio+correction)]
                    [void (iter curr next score-ratio correction accept)]
                    )
               (if accept
                 (loop next prop score iter)
                 (loop curr prop score iter))))

           (reset
             (list "MCMC run result:"
                   (loop init-state prop score iter))))


         )
