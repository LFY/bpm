(library (grammars)
         (export
           has-params?
           grammar->params
           nt->choices
           nt->name
           choice?
           grammar->nts
           grammar->tied-params)
         (import
           (rnrs)
           (program)
           (util)
           (_srfi :1))

         (define (grammar->params grammar+params)
           (cadddr grammar+params))

         (define (has-params? grammar)
           (not (null? (cdddr grammar))))

         (define (grammar->nts grammar+params)
           (append (program->abstractions grammar+params)
                   (list `(abstraction TopLevel () ,(caddr (program->body grammar+params))))))

         (define nt->name abstraction->name)
         (define (choice? body) (eq? 'choose (car body)))
         (define (nt->choices nt)
           (let* ([main-body (abstraction->pattern nt)])
             (cond [(choice? main-body) (cdr main-body)]
                   [else (list main-body)])))

         (define (grammar->tied-params grammar+params)
           (define nts-with-params
             (map (lambda (nt params)
                    (let* ([choices (nt->choices nt)])
                      `(abstraction
                         ,(nt->name nt)
                         ()
                         (choose ,@(map (lambda (param thunk) `(list ,param ,thunk)) params (map (lambda (choice) `(lambda () ,choice)) choices))))))
                  (grammar->nts grammar+params) (grammar->params grammar+params)))
           `(program
              ,nts-with-params
              (lambda () (TopLevel))))

         )
