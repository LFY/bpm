(library (query-learn)
         (export query-transforms
                 query-transform
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
                 (combinations))

         (define (query-transform prog abstr)
           (let* ([pred (learn-predicates prog abstr)]
                  [transformed-abstr (abstr->query prog abstr pred)]
                  [new-prog (program->replace-abstraction prog transformed-abstr)]
                  [res (remove-all-application-arguments new-prog abstr)])
             res))


         (define (query-transforms program . nofilter)
           (let* ([abstrs-with-vars (filter has-arguments? (program->abstractions program))]
                  [transformed-programs 
                    (delete '() (map (curry query-transform program) 
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

         ; Supporting functions



         (define (make-trivial-predicate vars) ; For testing purposes: a predicate accepting all assignments
           (make-named-abstraction (sym (predicate-symbol)) #t vars))

         ; As a program transformation: query-transform finds all abstraction
         ; with variables, and returns a program that replaces it one of them replaced
         ; with a query. For each candidate abstraction, it finds relevant
         ; predicates.

         (define (remove-all-application-arguments prog abstr)
           (define (remove-abstr-var-only var abstr) 
             (make-named-abstraction 
               (abstraction->name abstr) 
               (abstraction->pattern abstr) 
               (delete var (abstraction->vars abstr))))
           (define (remove-next var abstr-program) 
             (let* ([abstr (first abstr-program)]
                    [program (second abstr-program)])
               (list (remove-abstr-var-only var abstr) 
                     (remove-application-argument program abstr var))
               ))
           (second (fold remove-next (list abstr prog) (abstraction->vars abstr))))


         (define (deargument-all replacement-function prog abstr)
           (define (deargument-next v p)
             (deargument replacement-function p abstr v))
           (fold deargument-next prog (abstraction->vars abstr)))



         ; argument _vector_, which puts all applications in a uniform selection
         ; gateway to recursive calls to program merging function
         (define (uniform-arg-vector prog abstr)
           (let* ([vars (abstraction->vars abstr)]
                  [insts (map (curry find-variable-instances prog abstr) vars)])
             (map (curry uniform-replacement prog abstr) vars insts)
             ))

         ; for creating a list of defines, for use in query
         (define (mk-define l r)
           `(define ,l ,r))

         (define (mk-defines lhss rhss)
           (map mk-define lhss rhss))

         )

