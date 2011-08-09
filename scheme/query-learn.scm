(library (query-learn)
         (export query-transforms
                 query-transform
                 query-transform-substitute-equations
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
                  ;; [db (begin
                  ;;       (print "lhs-rhs: ~s" lhs-rhs)
                  ;;       (print "pattern-to-replace: ~s" pattern-to-replace)
                  ;;       (print "substitution ~s" substitution)
                  ;;       (print "body-before ~s" body-before)
                  ;;       (print "body-after ~s" body-after))]
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
                  
                  )
             (remove-application-arguments vars-to-remove 
                                           (program->replace-abstraction prog simplified-abstr)
                                           simplified-abstr)))

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
                    [transformed-program (remove-application-argument program abstr var)]
                    [final-program (program->replace-abstraction transformed-program transformed-abstr)])
               (list transformed-abstr final-program)))
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

