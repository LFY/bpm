(library (program)
         (export func-symbol var-symbol program-size var? func? make-abstraction make-named-abstraction abstraction->name abstraction->vars abstraction->pattern abstraction->define abstraction->variable-position make-program program->abstractions program->replace-abstraction capture-free-variables program->sexpr sexpr->program pretty-print-program program->body program->abstraction-applications define->abstraction set-indices-floor! make-program+ program+->program program+->posterior program+->log-likelihood program+->log-prior program+->semantics-preserved program+->program-transform has-variable? abstraction-application? program->abstraction-pattern any-abstraction-application?

                 sexpr-size

                 beta-reduce
                 get-free-vars
                 vars-in-body
                 pat->syms
                 program->lookup-abstraction
                 program->abstraction-applications-in-body

                 arg-matrix
                 prog->call-chains
                 arg-matrix-by-chain

                 arg-matrix-in-abstractions

                 find-variable-instances
                 find-variable-instances-in-body

                 ith-argument

                 )

         (import (except (rnrs) string-hash string-ci-hash)
                 (church readable-scheme)
                 (sym)
                 (_srfi :1)
                 (_srfi :69)
                 (util))

         ;;var-symbol and func-symbol are functions that return symbols so that they can be used in bher
         ;;think about moving these to a constants file since they're now separated due to unification-policies
         (define (func-symbol) 'F)
         (define (var-symbol) 'V)

         ;;language specific functions ;use reg-exps
         ;;temp fix b/c problems access to srfi 13
         (define (var? expr)
           (if (symbol? expr)
             (let* ([var-string (symbol->string (var-symbol))]
                    [string-expr (symbol->string expr)])
               ;; (string-prefix? var-pattern string-expr))))
               (equal? (substring string-expr 0 1) var-string))
             #f))

         (define (has-variable? sexpr)
           (if (deep-find var? sexpr) #t #f))

         (define (func? expr)
           (if (symbol? expr)
             (let* ([func-string (symbol->string (func-symbol))]
                    [string-expr (symbol->string expr)])
               ;; (string-prefix? var-pattern string-expr))))
               (equal? (substring string-expr 0 1) func-string))
             #f))

         (define (sexpr-size sexpr)
           (if (list? sexpr)
             (apply + (map sexpr-size sexpr))
             1))
         ;;compute the size of a program
         (define (program-size program)
           (define (prog? e) (and (list? e) (not (null? e)) (equal? 'program (car e))))
           (define (l-term? e) (and (list? e) (not (null? e)) (equal? 'lambda (car e))))
           (define (choice? e) (and (list? e) (not (null? e)) (equal? 'choose (car e))))

           (define (abstraction-size abstraction)
              (size (abstraction->pattern abstraction)))

           (define (lambda-size l-term)
             (size (caddr l-term)))

           (define (choice-size c-term)
             (size (cdr c-term)))

           (define (prog-size p)
             (+ (apply + (map abstraction-size (program->abstractions p)))
                (size (program->body p))))
           
           (define (size e)
             (cond [(l-term? e) (lambda-size e)]
                   [(choice? e) (choice-size e)]
                   [(pair? e) (+ (size (car e)) (size (cdr e)))]
                   [else 1]))
           (prog-size program))


           ;; (define (size program)
           ;;   (let* ([abstraction-sizes (apply + (map abstraction-size (program->abstractions program)))]
           ;;          [body-size (sexpr-size (program->body program))])
           ;;     (+ abstraction-sizes body-size))))



         ;;; data abstraction for abstraction 
         (define (make-abstraction pattern variables)
           (make-named-abstraction (sym (func-symbol)) pattern variables))
         (define (make-named-abstraction name pattern variables)
           (list 'abstraction name variables pattern))

         (define abstraction->name second)
         (define abstraction->vars third)
         (define abstraction->pattern fourth)

         ;; next: find define 

         ;; make a define statement out of an abstraction, format is (define name (lambda (vars) body))
         (define (abstraction->define abstraction)
           (let ((name (abstraction->name abstraction))
                 (variables (abstraction->vars abstraction))
                 (pattern (abstraction->pattern abstraction)))
             (list 'define name (list 'lambda variables pattern))))

         (define (abstraction->variable-position abstraction variable)
           (list-index (lambda (x) (equal? variable x)) (abstraction->vars abstraction)))

         (define (abstraction-application? abstraction sexpr)
           (if (non-empty-list? sexpr)
             (if (equal? (first sexpr) (abstraction->name abstraction))
               #t
               #f)
             #f))

         (define (any-abstraction-application? sexpr)
           (if (non-empty-list? sexpr)
             (func? (first sexpr))
             #f))

         (define (get-free-vars abstraction)
           (let* ([pattern (abstraction->pattern abstraction)]
                  [non-free (abstraction->vars abstraction)]
                  [free '()]
                  [free-var? (lambda (x) (and (var? x) (not (member x non-free))))]
                  [add-to-free! (lambda (x) (set! free (pair x free)))])
             (sexp-search free-var? add-to-free! pattern)
             free))

         (define (vars-in-body abstraction)
           (let* ([pattern (abstraction->pattern abstraction)]
                  [non-free '()]
                  [free '()]
                  [free-var? (lambda (x) (and (var? x) (not (member x non-free))))]
                  [add-to-free! (lambda (x) (set! free (pair x free)))])
             (sexp-search free-var? add-to-free! pattern)
             free))

         (define (pat->syms pattern)
           (let* (
                  [free '()]
                  [free-var? (lambda (x) (var? x))]
                  [add-to-free! (lambda (x) (set! free (pair x free)))])
             (sexp-search free-var? add-to-free! pattern)
             free))

         ;;free variables can occur when the pattern for an abstraction contains variables that were part of the matched expressions e.g. if the expression was (+ v1 v1 a) (+ v1 v1 b) then the pattern would be (+ v1 v1 v2)
         ;;we "capture" the variables by adding them to the function definition
         ;;an alternative approach would be to have nested abstractions

         (define (capture-free-variables abstraction)
           (let* ([free-vars (get-free-vars abstraction)])
             (if (null? free-vars)
               abstraction
               (let* ([new-vars (append free-vars (abstraction->vars abstraction))] 
                      [old-pattern (abstraction->pattern abstraction)]
                      ;;add new-pattern with new variable names for captured-vars to prevent isomorphic abstractions
                      [old-name (abstraction->name abstraction)]
                      [no-free-abstraction (make-named-abstraction old-name old-pattern new-vars)])
                 no-free-abstraction))))

         ;;;data abstraction for programs
         (define (make-program abstractions body)
           (list 'program abstractions body))
         (define program->abstractions second)
         (define program->body third)

         (define (program->lookup-abstraction program abstraction-name)
           (define (find-abstraction abstractions name)
             (if (null? abstractions)
               (error "abstraction not found" name)
               (let* ([current-abstraction (first abstractions)])
                 (if (eq? (abstraction->name current-abstraction) abstraction-name)
                   current-abstraction
                   (find-abstraction (rest abstractions) name)))))
           (find-abstraction (program->abstractions program) abstraction-name))


         (define (program->abstraction-pattern program abstraction-name)
           (abstraction->pattern (program->lookup-abstraction program abstraction-name)))

         ;;it might also be possible to search over program->sexpr, but then we'd need a more complicated predicate to avoid the definition of the target-abstraction
         (define (program->abstraction-applications program target-abstraction)
           (define (target-abstraction-application? sexpr)
             (if (non-empty-list? sexpr)
               (if (equal? (first sexpr) (abstraction->name target-abstraction))
                 #t
                 #f)
               #f))
           (let* ([abstraction-patterns (map abstraction->pattern (program->abstractions program))]
                  [possible-locations (pair (program->body program) abstraction-patterns)])
             (deep-find-all target-abstraction-application? possible-locations)))

         (define (program->abstraction-applications-in-body program target-abstraction)
           (define (target-abstraction-application? sexpr)
             (if (non-empty-list? sexpr)
               (if (equal? (first sexpr) (abstraction->name target-abstraction))
                 #t
                 #f)
               #f))
           (let* (
                  [possible-locations (cons (program->body program) '())])
             (deep-find-all target-abstraction-application? possible-locations)))

         (define (program->abstraction-applications-in-abstractions program target-abstraction)
           (define (target-abstraction-application? sexpr)
             (if (non-empty-list? sexpr)
               (if (equal? (first sexpr) (abstraction->name target-abstraction))
                 #t
                 #f)
               #f))
           (let* ([abstraction-patterns (map abstraction->pattern (program->abstractions program))]
                  [possible-locations abstraction-patterns])
             (deep-find-all target-abstraction-application? possible-locations)))

         ;;assumes the new-abstraction has the same name as the abstraction it is replacing in program
         ;;assumes a particular abstraction is only defined once in the program
         (define (program->replace-abstraction program new-abstraction)
           (define (replace-abstraction abstractions new-abstraction)
             (if (null? abstractions)
               '()
               (let* ([current-abstraction (first abstractions)])
                 (if (equal? (abstraction->name current-abstraction) (abstraction->name new-abstraction))
                   (pair new-abstraction (rest abstractions))
                   (pair current-abstraction (replace-abstraction (rest abstractions) new-abstraction))))))
           (let* ([abstractions (program->abstractions program)]
                  [new-abstractions (replace-abstraction abstractions new-abstraction)])
             (make-program new-abstractions (program->body program))))

         (define (program->sexpr program)
           `(let ()  
              ,@(map abstraction->define (program->abstractions program))
              ,(program->body program)))

         ;;assumes format of (let () definitions body); if format fails to hold then program is an empty set of abstractions and the sexpr as the body
         (define (sexpr->program sexpr)
           (define (abstraction-sexpr? x)
             (if (and (not (null? x)) (list? x))
               (equal? (first x) 'define)
               #f))
           (let*-values ([(no-scope-sexpr) (remove-scope sexpr)]
                         [(abstractions body) (span abstraction-sexpr? no-scope-sexpr)])
                        (make-program (map define->abstraction abstractions) (first body))))

         (define (remove-scope sexpr)
           (define (scope? x)
             (or (equal? 'let x) (null? x)))
           (let*-values ([(scope program) (span scope? sexpr)])
                        program))

         (define (pretty-print-program program)
           (let ([sexpr (program->sexpr program)])
             (pretty-print sexpr)
             ;;(for-each display (list "size: " (program-size sexpr) "\n\n"))))
             (for-each display (list "size (used in prior): " (program-size program) "\n\n"))))

         ;;define is of the form (define name (lambda (vars) body))
         (define (define->abstraction definition)
           (let* ([name (second definition)]
                  [vars (second (third definition))]
                  [body (third (third definition))])
             (make-named-abstraction name body vars)))

         ;;used to ensure all function and variable names are in consecutive order; important for when trying to generate a program from a grammar that matches a compressed program
         (define (normalize-names expr)
           (define ht (make-hash-table eqv?))
           (define (traverse action expr)
             (if (or (primitive? expr) (null? expr))
               (if (or (func? expr) (var? expr))
                 (action expr)
                 expr)
               (map (curry traverse action) expr)))
           ;;build table
           (define (add-to-table expr)
             (if (func? expr)
               (hash-table-set! ht expr (sym (func-symbol)))
               (hash-table-set! ht expr (sym (var-symbol)))))
           (define (relabel expr)
             (hash-table-ref ht expr))
           (reset-symbol-indizes!)
           (let* ([signatures (sexpr->signatures expr)])
             (traverse add-to-table signatures))
           (traverse relabel expr))
         (define (sexpr->signatures sexpr)
           (let* ([program (sexpr->program sexpr)]
                  [defs (program->abstractions program)]
                  [names (map abstraction->name defs)]
                  [vars (map abstraction->vars defs)])
             (map pair names vars)))

         ;;increases the current symbol to the highest 
         (define (set-indices-floor! expr)
           (let ([funcs (find-tagged-symbols expr (func-symbol))]
                 [vars (find-tagged-symbols expr (var-symbol))])
             (begin
               (raise-tagged! (func-symbol) funcs)
               (raise-tagged! (var-symbol) vars))))

         ;;used in calculation of the posterior
         (define (make-program+ program posterior log-likelihood log-prior semantics-preserved)
           (list 'program+ program posterior log-likelihood log-prior semantics-preserved))
         (define program+->program second)
         (define program+->posterior third)
         (define program+->log-likelihood fourth)
         (define program+->log-prior fifth)
         (define program+->semantics-preserved sixth)
         (define (program+->program-transform semantics-preserved program+ new-program)
           (make-program+ new-program (program+->posterior program+) (program+->log-likelihood program+) (program+->log-prior program+) semantics-preserved))


         ; takes an abstraction to its pattern, substituting args for its variables.
         (define (beta-reduce abstr args)
           (define (reduce-one abstr var val)
             (let* ([new-name (abstraction->name abstr)]
                    [new-vars (delete var (abstraction->vars abstr))]
                    [new-pattern (sexp-replace var val (abstraction->pattern abstr))]
                    [res (make-named-abstraction new-name new-pattern new-vars)])
               res))
           (define (reduce-next next-var-val curr-abs)
             (reduce-one curr-abs (first next-var-val) (second next-var-val)))
           (let* ([var-vals (zip (abstraction->vars abstr) args)])
             (abstraction->pattern (fold reduce-next abstr var-vals))))

         ; the complete arg x call matrix
         (define (arg-matrix prog abstr)
           (let* ([vars (abstraction->vars abstr)]
                  [insts (map (curry find-variable-instances-in-body prog abstr) vars)])
             insts
             ))

         (define (arg-matrix-in-abstractions prog abstr)
           (let* ([vars (abstraction->vars abstr)]
                  [insts (map (curry find-variable-instances-in-abstractions prog abstr) vars)])
             insts
             ))


         ;; a list of argxcall matrices, grouped by _chain_
         (define (prog->call-chains prog abstr)
           (let* ([name (abstraction->name abstr)]
                  [body (program->body prog)]
                  [abstrs (program->abstractions prog)]
                  [chains (shallow-find-all (lambda (x) (eq? name x)) body)]
                  [mk-chain-prog (lambda (b) (make-program abstrs b))])
             (map mk-chain-prog chains)))

         (define (arg-matrix-by-chain prog abstr)
           (let* ([chains (prog->call-chains prog abstr)]
                  ;; [db (begin (display "in arg-matrix-by-chain: chains: ") (display chains) (display "\n"))]
                  ;; [nontrivial-chains (filter (lambda (x) (< 1 (length x))) chains)]
                  )
             (map (lambda (p) (arg-matrix p abstr)) chains)))

         (define (find-variable-instances program abstraction variable)
           (let* ([abstraction-applications (program->abstraction-applications program abstraction)]
                  [variable-position (abstraction->variable-position abstraction variable)]
                  [variable-instances (map (curry ith-argument variable-position) abstraction-applications)])
             variable-instances))

         (define (find-variable-instances-in-body program abstraction variable)
           (let* ([abstraction-applications (program->abstraction-applications-in-body program abstraction)]
                  [variable-position (abstraction->variable-position abstraction variable)]
                  [variable-instances (map (curry ith-argument variable-position) abstraction-applications)])
             variable-instances))

         (define (find-variable-instances-in-abstractions program abstraction variable)
           (let* ([abstraction-applications (program->abstraction-applications-in-abstractions program abstraction)]
                  [variable-position (abstraction->variable-position abstraction variable)]
                  [variable-instances (map (curry ith-argument variable-position) abstraction-applications)])
             variable-instances))

         (define (ith-argument i function-application)
           (list-ref function-application (+ i 1)))
         )
