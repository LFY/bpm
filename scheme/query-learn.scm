(library (query-learn)
         (export query-transforms
                 query-transforms-noisy

                 query-transform
                 query-transform-substitute-equations
                 query-recursion-transforms

                 query-transform-substitute-equations-noisy
                 query-recursion-transforms-noisy

                 abstr->query)
         (import (except (rnrs) string-hash string-ci-hash)
                 (church readable-scheme)
                 (sym)
                 (dearguments)
                 (prob-logic)
                 (program)
                 (_srfi :1)
                 (_srfi :69)
                 (util)
                 (printing)
                 (combinations))

         (define (query-transforms program . nofilter)
           (let* ([abstrs-with-vars (filter has-arguments? (program->abstractions program))]
                  [transformed-programs 
                    (delete '() (map (curry query-transform-substitute-equations program) 
                                     abstrs-with-vars))] 
                  [prog-size (program-size program)]
                  [valid-transformed-programs
                    (if (not (null? nofilter))
                      transformed-programs
                      (filter (lambda (ip) (< (program-size ip)
                                              prog-size))
                              transformed-programs))])
             valid-transformed-programs))

         (define (query-transforms-noisy program . nofilter)
           (let* ([abstrs-with-vars (filter has-arguments? (program->abstractions program))]
                  [transformed-programs 
                    (delete '() (map (curry query-transform-substitute-equations-noisy program) 
                                     abstrs-with-vars))] 
                  [prog-size (program-size program)]
                  [valid-transformed-programs
                    (if (not (null? nofilter))
                      transformed-programs
                      (filter (lambda (ip) (< (program-size ip)
                                              prog-size))
                              transformed-programs))])
             valid-transformed-programs))

         (define (query-transform prog abstr)
           (let* ([pred (learn-predicates prog abstr)]
                  [transformed-abstr (abstr->query prog abstr pred)]
                  [new-prog (program->replace-abstraction prog transformed-abstr)]
                  [res (remove-all-application-arguments new-prog abstr)])
             res))

         (define (substitute-one-pattern lhs-rhs abstr)
           (let* ([pattern-to-replace (first lhs-rhs)]
                  [substitution (second lhs-rhs)]
                  [body-before (abstraction->pattern abstr)]
                  [body-after (sexp-replace pattern-to-replace substitution body-before)]
                  )
             (make-named-abstraction (abstraction->name abstr) ; same name as old abstraction
                                     body-after
                                     (abstraction->vars abstr))))

         (define (query-transform-substitute-equations prog abstr)
           (let* ([pred (learn-predicates-keep-vars prog abstr)]
                  [substitutions (generate-substitutions pred)]
                  [vars-before (abstraction->vars abstr)]
                  [simplified-abstr (fold substitute-one-pattern abstr substitutions)] 
                  [vars-after (vars-in-body simplified-abstr)]
                  [vars-to-remove (list-subtract vars-before vars-after)]
                  ;; [db (begin
                        ;; (print "predicate: ~s" pred)
                        ;; (print "substitutions: ~s" substitutions)
                        ;; (print "vars-before ~s" vars-before)
                        ;; (print "vars-after ~s" vars-after)
                        ;; (print "vars-to-remove ~s" vars-to-remove)
                        ;; (print "current abstr ~s" abstr)
                        ;; (print "simplified abstr ~s" simplified-abstr))]
                  )
             (remove-application-arguments vars-to-remove 
                                           (program->replace-abstraction prog simplified-abstr)
                                           simplified-abstr)))

         (define (query-transform-substitute-equations-noisy prog abstr) 
           (let* ([substitutions (learn-noisy-substitutions prog abstr)]
                  [vars-before (abstraction->vars abstr)]
                  [simplified-abstr (fold substitute-one-pattern abstr substitutions)] 
                  [vars-after (vars-in-body simplified-abstr)]
                  [vars-to-remove (list-subtract vars-before vars-after)]
                  )
             (remove-application-arguments vars-to-remove 
                                           (program->replace-abstraction prog simplified-abstr)
                                           simplified-abstr)))
                                                            
         (define query-recursion-transforms-noisy (make-dearguments-transformation predicate-recursion-replacement-noisy))

         (define query-recursion-transforms (make-dearguments-transformation predicate-recursion-replacement))

         (define (predicate-recursion-replacement program abstraction variable variable-instances)
           (predicate-recursion-replacement-gen get-column-predicates program abstraction variable variable-instances))

         (define (predicate-recursion-replacement-noisy program abstraction variable variable-instances)
           (predicate-recursion-replacement-gen get-column-predicates-noisy program abstraction variable variable-instances))

         (define (predicate-recursion-replacement-gen substitution-fx program abstraction variable variable-instances)
           (define (mk-recursive-call program abstraction)
             (let* (
                    [recursion-substitutions (substitution-fx program abstraction)]
                    [find-recursion-substitution 
                      (lambda (var)
                        (if (not (assq var recursion-substitutions)) var
                          (let* ([search-result (assq var recursion-substitutions)]
                                 [local-rec-vars (second search-result)]
                                 [equations (third search-result)]
                                 [my-level-var (first local-rec-vars)]
                                 [next-level-var (second local-rec-vars)]
                                 [candidate-substitutions
                                   (map (lambda (e)
                                          (cond [(eq? next-level-var (first e)) (second e)]
                                                [else (first e)])) 
                                        (filter (lambda (e)
                                             (and (contains? next-level-var e)
                                                  (contains? my-level-var
                                                             (concatenate (map (lambda (pat) (pat->syms pat)) e)))))
                                           equations))])
                            (if (null? candidate-substitutions) var
                              (sexp-replace-pred var? var (first candidate-substitutions))))

                          ))])
               `(,(abstraction->name abstraction)
                  ,@(map find-recursion-substitution (abstraction->vars abstraction)))))
               
           (let* ([valid-variable-instances (remove has-variable? variable-instances)]
                  ;; [db (begin (display "valid-variable-instances:\n") (display valid-variable-instances) (display "\n"))]
                  [recursive-calls (filter (curry abstraction-application? abstraction) valid-variable-instances)]
                  ;; [db (begin (display "recursive-calls:\n") (display recursive-calls) (display "\n"))]
                  [non-recursive-calls (filter (lambda (x) (not (abstraction-application? abstraction x)))
                                                                  valid-variable-instances)]
                  ;; [db (begin (display "non-recursive-calls:\n") (display non-recursive-calls) (display "\n"))]
                  [terminates (terminates? program (abstraction->name abstraction) non-recursive-calls)]
                  ;; [db (begin (display "terminates:\n") (display terminates) (display "\n"))]
                  )
             (if (or (null? valid-variable-instances) (null? recursive-calls) (not terminates))
               NO-REPLACEMENT
               (let* ([prob-of-recursion (/ (length recursive-calls) (length valid-variable-instances))])
                 `(if (flip ,prob-of-recursion) ,(mk-recursive-call program abstraction) (uniform-choice  ,@non-recursive-calls))))))



         ; abstr->query: fully deargumenting an abstraction in one step
         ; converts an abstraction to a query by:
         ; 1. finding all of its applications
         ; 2. putting the instances of each argument into a uniform distribution
         ; 3. the final query is the old abstraction with the specified predicate as conditioning statement
         (define (abstr->query prog abstr pred)
           (let* ([fam (uniform-arg-vector prog abstr)] ; a list of distributions
                  [smp-syms (map (lambda (x) (sym (sample-symbol))) fam)] ; sample symbols
                  [query-defines (mk-defines smp-syms fam)] ; sampling from the distributions
                  [query-body (beta-reduce abstr smp-syms)] ; returning the body of the original abstraction
                  [query-predicate (beta-reduce pred smp-syms)])
             (make-named-abstraction (abstraction->name abstr) ; same name as old abstraction
                                     `(query ; the actual query body
                                        ,@query-defines
                                        ,query-body
                                        ,query-predicate)
                                     '()) ; full deargumentation

             ))

         (define (remove-all-application-arguments prog abstr)
           (remove-application-arguments (abstraction->vars abstr) prog abstr))

         (define (remove-application-arguments vars prog abstr)
           (define (remove-abstr-var-only var abstr) 
             (make-named-abstraction 
               (abstraction->name abstr) 
               (abstraction->pattern abstr) 
               (delete var (abstraction->vars abstr))))

           (define (remove-next var abstr-program) 
             (let* ([abstr (first abstr-program)]
                    [program (second abstr-program)]
                    [transformed-abstr (remove-abstr-var-only var abstr)]
                    [transformed-program (program->replace-abstraction program transformed-abstr)]
                    [final-program (remove-application-argument transformed-program abstr var)])
               (begin ;; (print "input abstraction:")
                      ;; (pretty-print abstr)
                      ;; (print "transformed-program:")
                      ;; (pretty-print transformed-program)
                      ;; (print "final-program: ")
                      ;; (pretty-print final-program)
                      (list transformed-abstr final-program))
               ))
           (second (fold remove-next (list abstr prog) vars)))

         (define (uniform-arg-vector prog abstr)
           (let* ([vars (abstraction->vars abstr)]
                  [insts (map (curry find-variable-instances prog abstr) vars)])
             (map (curry uniform-replacement prog abstr) vars insts)
             ))

         (define (mk-define l r)
           `(define ,l ,r))

         (define (mk-defines lhss rhss)
           (map mk-define lhss rhss))

         )

