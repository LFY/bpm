(library (generators)
         (export 
           yield->
           forin-yield->
           forin->
           next-<
           begin-<
           )

         (import (rnrs)
                 (delimcc-simple-ikarus)
                 )

         (define (next-< stream)
           (cond [(eq? 'End stream) 'End]
                 [else
                   (let*
                     ([val-cont (reset ((cdr stream) 'End))])
                     (cond [(eq? 'End val-cont) 'End]
                           [else (let* 
                                   ([val (car val-cont)]
                                    [cont (cdr val-cont)])
                                   (cons val cont))]))]))

         (define (forin-> f stream)
           (define (loop val-cont)
             (cond [(eq? 'End val-cont) 'End]
                   [else
                     (let* ([val (car val-cont)]
                            [next-val-cont (next-< val-cont)])
                       (begin (f val)
                              (loop next-val-cont)))]))
           (loop (begin-< stream)))


         (define (forin-yield-> stream)
           (define (loop val-cont)
             (cond [(eq? 'End val-cont) 'End]
                   [else
                     (let* ([next-val-cont (next-< val-cont)])
                       (begin (yield-> (car next-val-cont))
                              (loop next-val-cont)))]))
           (loop (begin-< stream)))


         (define (begin-< stream)
           (let* ([start-val-cont (reset (stream))])
             start-val-cont))

         (define (yield-> val)
           (shift k (cons val k)))


         )
                 
