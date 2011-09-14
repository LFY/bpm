(library (sxml2scfg)
         (export sxml->nts
                 sxmls->nts
                 sxmls->initial-program)
         (import (except (rnrs) string-hash string-ci-hash)
                 (node-constructors)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)
                 (util)
                 (delimcc-simple-ikarus)
                 (program)
                 )

         (define nt-defs '())
         (define nt-counter 0)

         (define (gen-nt-sym)
           (let* ([answer (string->symbol (string-append "F" (number->string nt-counter)))])
             (begin (set! nt-counter (+ 1 nt-counter))
                    answer)))

         (define nt-def->name second)
         (define nt-def->body third)

         (define (nt-start store e)
           (let* ([new-def (reset `(define (,(gen-nt-sym)) ,e))]
                  [new-name (nt-def->name new-def)])
             (begin (hash-table-set! store new-name new-def)
                    new-name)))

         (define (nt-end e) (shift k (begin (k e) e)))

         (define (nt-boundary store e) (nt-end (nt-start store e)))


         (define (sxml->nts pred? store expr)
           (define (loop e)
             (cond [(null? e) '()]
                   [(primitive? e) e]
                   [(pred? e) (nt-boundary store (cons (loop (car e)) (loop (cdr e))))]
                   [(list? e) (cons (loop (car e)) 
                                    (loop (cdr e)))]))
           (loop expr))


         (define (sxmls->nts pred? sxmls)
           (define store (make-hash-table equal?))
           (list (map (curry sxml->nts pred? store) sxmls) 
                 (map cdr (hash-table->alist store))))

         (define scfg->top-nts car)
         (define scfg->nt-defs cadr)

         (define (sxmls->initial-program nt-pred? sxmls)
           (let* ([scfg (sxmls->nts nt-pred? sxmls)]
                  [prog-body `(lambda () (choose ,@(scfg->top-nts scfg)))]
                  [nt-def->abstraction (lambda (nt) (make-named-abstraction (car (nt-def->name nt))
                                                                            (nt-def->body nt)
                                                                            '()))]
                  [abstractions (map nt-def->abstraction (scfg->nt-defs scfg))])
             (make-program abstractions prog-body)))

         )
