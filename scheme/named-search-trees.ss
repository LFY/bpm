;; Library for converting unfold program into SCFG over trees

;; The resulting SCFG can be considered a "stochastic search tree" in the sense
;; of HANSEI, except with names, so we can get recursion and sharing of
;; definitions.  Hence "named search tree."

(library (named-search-trees)
         (export define-nondet-prog
                 define-nondet
                 nondet-choice
                 nondet-program->named-search-tree
                 
                 ;; debug
                 make-root
                 reify-choice-context
                 make-choice-NT)


         (import (except (rnrs) string-hash string-ci-hash)
                 (printing)
                 (util)
                 (_srfi :69)
                 (_srfi :1)
                 (delimcc-simple-ikarus)
                 )

         (define (make-root name thunk)
           (reset ((lambda ()
                              `(define-NT ,name ,(thunk))))))

         (define (mk-choice-context name counter term)
           `(Choice-Context ,name ,counter ,term))

         ;; name, 0 essentially act as a form of dynamically-scoped local
         ;; storage, we throw these away when the function returns
         (define (reify-choice-context name thunk)
           (choice-context->term (reset (mk-choice-context name 0 (thunk)))))

         (define choice-context->name cadr)
         (define choice-context->counter caddr)
         (define choice-context->term cadddr)

         (define (increment-choice-context cc)
           (mk-choice-context (choice-context->name cc)
                              (+ 1 (choice-context->counter cc))
                              (choice-context->term cc)))
                                                    
         (define (appname-counter->choice-symbol name counter)
           (list 'C counter name))

         (define (choice-context->hash cc)
           (appname-counter->choice-symbol (choice-context->name cc)
                                           (choice-context->counter cc)))

         (define nt-defs (make-hash-table equal?))
         (define symbol-store (make-hash-table equal?))

         (define choice-symbol-count 0)
         (define (choice-symbol)
           (begin (set! choice-symbol-count (+ 1 choice-symbol-count))
                  (string->symbol (string-append "choice" (number->string choice-symbol-count)))))
         
         (define (gen-or-retrieve-symbol store context-name)
           (if (hash-table-exists? store context-name)
             (hash-table-ref store context-name)
             (begin 
               (hash-table-set! store context-name (choice-symbol))
               (hash-table-ref store context-name))))

         (define (store-get-def context-name gen-context-def) 
           (if (hash-table-exists? nt-defs context-name)
             (hash-table-ref nt-defs context-name)
             (begin (hash-table-set! nt-defs context-name 'PRIMED) ;; Yes, this line is necessary.
                    (hash-table-set! nt-defs context-name (gen-context-def))
                    (hash-table-ref nt-defs context-name))))

         ;; make-choice-NT: stores the definition of the nonterminal corresponding to this choice,
         ;; and returns the name (as the context + abstraction in question).

         (define (construct-NT name body . vars)
           `(define ,(cons name vars) ,body))

         (define h-counter 0)

         (define (sym)
           (let ([answer (string->symbol (string-append "H" (number->string h-counter)))])
             (begin (set! h-counter (+ 1 h-counter))
                    answer)))

         (define (choice-context->vars cc)
           (define (sample-name? t)
             (and (symbol? t)
                  (equal? "H" (substring (symbol->string t) 0 1))))
           (filter sample-name? (cdr (choice-context->name cc))))


         (define (make-choice-NT . choices)
           (shift k ;; k: the reified partial continuation: (Choice-Context hash counter term[])
                   (let* (
                          [sample-name (sym)]

                          ;; increment choice context is for the different choices occuring in same context. or something.
                          ;; [choice-context (increment-choice-context (k sample-name))] 
                          [choice-context (increment-choice-context (k sample-name))]

                          ;; in general we won't be able to recover the
                          ;; structure ; not every ADT allows us to a 'H there.
                          ;; however, it may be possible under a symbolic
                          ;; execution of the program, where context is the program trace.

                          ;; rule: if we see H on the LHS, that means a random
                          ;; choice flowed into this function, so we will have
                          ;; to 'delay' the final definition---or use the
                          ;; sample name in the definition of the "nonterminal"

                          [context-name (gen-or-retrieve-symbol symbol-store (choice-context->hash choice-context))]
                          [context-vars (choice-context->vars choice-context)]
                          
                          [gen-context-def (lambda () 
                                             (apply (curry construct-NT context-name 
                                                           `(choose ,@(map (lambda (f) (f)) choices)))
                                                    (choice-context->vars choice-context)))]
                                             ;; `(define ,context-name 
                                                         ;; (choose ,@(map (lambda (f) (f)) choices))))]
                          ;; [answer (increment-choice-context (k `(,context-name)))]
                          
                          [context-app `(,context-name ,@context-vars)]

                          ;; version taking sharing into account:
                          ;; [answer (let* ([final-context (increment-choice-context (k sample-name))])
                                    ;; (mk-choice-context (choice-context->name final-context)
                                                       ;; (choice-context->counter final-context)
                                                       ;; `(let ([,sample-name ,context-app])
                                                          ;; ,(choice-context->term final-context))))]


                          ;; old version
                          [answer (increment-choice-context (k context-app))]

                          ;; [answer (increment-choice-context
                                    ;; `(let ([,sample-name ,context-app])
                                       ;; ,(k sample-name)))]
                          ;; [answer `(let ([,sample-name ,context-app])
                                     ;; ,(increment-choice-context (k sample-name)))]

                          )
                     (begin
                       (store-get-def context-name gen-context-def)

                      ;(begin (print "in make-choice-NT:======================")
                      ;       (print "my choice context:")
                      ;       (pretty-print choice-context)
                      ;       (print "my context name:")
                      ;       (pretty-print context-name)
                      ;       (newline)
                      ;       (print "current symbol store:")
                      ;       (pretty-print (hash-table->alist symbol-store))
                      ;       (print "current definition store:")
                      ;       (pretty-print (hash-table->alist nt-defs))
                      ;       (newline)
                      ;       (print "final choice context:")
                      ;       (pretty-print answer)
                      ;       (print "end make-choice-NT=====================")
                      ;       (newline)
                      ;       (newline))
                       answer
                       ))))



         ;; we can define "direct-style" programs that transform to SCFGs 
         (define-syntax define-nondet-prog
           (syntax-rules ()
                         ((define-nondet-prog (name . vars) body)
                          (define (name . vars)
                            (make-root 'start (lambda () body))))))

         (define-syntax define-nondet
           (syntax-rules ()
                         ((define-nondet (name . vars) body)
                          (define (name . vars)
                            (reify-choice-context (list 'name . vars) (lambda () body))))))

         (define-syntax process-choices
           (syntax-rules ()
                         [(process-choices) '()]
                         [(process-choices e1 e2 ...) (cons (lambda () e1) (process-choices e2 ...))]
                         ;; ((process-choices e) (lambda () e))
                         ))

         (define-syntax nondet-choice
           (syntax-rules ()
                         ((nondet-choice . xs) (apply make-choice-NT (process-choices . xs)))
                         ;; ((nondet-choice e1) (make-choice-NT (lambda () e1)))
                         ;; ((nondet-choice e1 e2 ...) (make-choice-NT (process-choices e1 e2 ...)))
           
           ))


         ;; Converting a program to named search tree, i.e., stochastic context free tree grammar
         ;; there is one nonterminal per random choice (modulo naming)

         (define (nondet-program->named-search-tree thunk)
           (begin
             ;; clear the store
             (set! nt-defs (make-hash-table equal?))

             ;; run the program, return the results
             (let* ([start-tree (thunk)]
                    [definitions (map cdr (hash-table->alist nt-defs))])
               (list (construct-NT 'start start-tree) definitions))

             ))

         
         )
