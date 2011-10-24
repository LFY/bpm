(library (sxml2scfg)
         (export sxml->nts
                 sxmls->nts
                 sxmls->initial-program
                 sxmls->initial-program-keep-dups

                 remove-duplicate-rhs
                 )
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

         (define (remove-all-dup-rhs scfg)
           (define (loop prev)
             (let* ([next (remove-duplicate-rhs scfg)])
               (cond [(equal? prev next) prev]
                     [else (loop next)])))
           (loop scfg))


         (define (remove-duplicate-rhs scfg)

           (define hash->rep (make-hash-table equal?))
           (define name->rep (make-hash-table equal?))

           (define (nt->rep nt)
             (let* ([body (nt-def->body nt)]
                    [name (nt-def->name nt)]
                    [rep-name (hash-table-ref hash->rep
                                              body
                                              (lambda ()
                                                (begin
                                                  (hash-table-set! hash->rep body name)
                                                  (hash-table-set! name->rep name name)
                                                  name)))])
               (begin (hash-table-set! name->rep name rep-name)
                      rep-name)))


           (define all-nt-names (map nt-def->name (scfg->nt-defs scfg)))

           (define (rename-def nt)
             (let* ([new-body
                      (subexpr-walk (lambda (t)
                                      (cond [(contains? t all-nt-names)
                                             (hash-table-ref name->rep t)]
                                            [else t]))
                                    (nt-def->body nt))]
                    [new-name (hash-table-ref name->rep (nt-def->name nt))])
               `(define ,new-name ,new-body)))

           (let* ([defs (scfg->nt-defs scfg)])
             (begin ;; (pretty-print scfg)
                    ;; (pretty-print defs)
                    (map nt->rep defs)

                      `(,(delete-duplicates (map (lambda (name) (hash-table-ref name->rep name))
                                               (scfg->top-nts scfg)))
                       ,(delete-duplicates (map rename-def defs)))
                    )))


         (define (sxmls->initial-program nt-pred? sxmls)
           (let* (
                  [scfg (remove-all-dup-rhs (sxmls->nts nt-pred? sxmls))]
                  ;; [scfg (sxmls->nts nt-pred? sxmls)]
                  [prog-body `(lambda () (choose ,@(scfg->top-nts scfg)))]
                  [nt-def->abstraction (lambda (nt) (make-named-abstraction (car (nt-def->name nt))
                                                                            (nt-def->body nt)
                                                                            '()))]
                  [abstractions (map nt-def->abstraction (scfg->nt-defs scfg))])
             (make-program abstractions prog-body)))

         (define (sxmls->initial-program-keep-dups nt-pred? sxmls)
           (let* (
                  [scfg (sxmls->nts nt-pred? sxmls)]
                  [prog-body `(lambda () (choose ,@(scfg->top-nts scfg)))]
                  [nt-def->abstraction (lambda (nt) (make-named-abstraction (car (nt-def->name nt))
                                                                            (nt-def->body nt)
                                                                            '()))]
                  [abstractions (map nt-def->abstraction (scfg->nt-defs scfg))])
             (make-program abstractions prog-body)))
         )
