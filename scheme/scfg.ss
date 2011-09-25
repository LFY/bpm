(library (scfg)
         (export scfg->axiom
                 scfg->productions
                 scfg->nonterminal-names

                 prod->name
                 prod->successors
                 prod->body

                 name->prod

                 prod->choices

                 nondet?

                 nt-name->symbol

                 rename-successors

                 prefix-scfg

                 scfg->start-name
                 )
         (import (rnrs)
                 (_srfi :1)
                 (prolog-serialize)
                 (util)
                 (printing)
                 )

         ;; SCFG: (list <axiom> (list <productions> ))

         ;; each production:
         ;; (define <name> <body>)

         ;; each 

         (define prod->name second)
         (define prod->body third)

         (define scfg->axiom first)

         (define (scfg->productions scfg)
           (cons (scfg->axiom scfg) (second scfg)))

         (define (scfg->nonterminal-names scfg)
           (map prod->name (scfg->productions scfg)))

         (define (prod->successors scfg prod)
           (deep-find-all (lambda (x) (contains? x (scfg->nonterminal-names scfg))) (prod->body prod)))

         (define (nondet? prod)
           (eq? 'choose (car (prod->body prod))))

         (define (prod->choices prod)
           (cond [(nondet? prod) (cdr (prod->body prod))]
                 [else (list (prod->body prod))]))

         (define (name->prod scfg name)
           (let* ([result (filter (lambda (p) (eq? name (prod->name p))) (scfg->productions scfg))])
             (cond [(null? result) '()]
                   [else (car result)])))

         (define nt-name->symbol car)


         (define (substitute-body body nt-vals)
           (define (replace-next nt-val current-body)
             (let* ([nt-name (first nt-val)]
                    [val (second nt-val)])
               (sexp-replace nt-name val current-body)))
           (fold replace-next body nt-vals))

         ;; outputs list of bodies with successors renamed according to name-map
         (define (rename-successors scfg prod name-map)
           (map (lambda (body) (substitute-body body
                                                (map (lambda (v) (list v (name-map v)))
                                                     (prod->successors scfg prod))))
                (prod->choices prod)))

         ;; attaches a prefix to all names of productions in scfg.
         ;; useful for batch processing

         ;; prefix is a string that must be able to work as a symbol

         (define (prefix-scfg prefix scfg)
           (define (transform-name name) (replace-car name (string->symbol (string-append prefix (symbol->string (car name))))))
           (define (is-NT? name) (and (non-empty-list? name) (symbol? (car name))
                                      (or (equal? "start" (symbol->string (car name)))
                                          (and (> (string-length (symbol->string (car name))) 6) (equal? "choice" (substring (symbol->string (car name)) 0 6))))))
           (subexpr-walk (lambda (t) (cond [(is-NT? t) (transform-name t)]
                                           [else t]))
                         scfg))


         (define (scfg->start-name scfg)
           (prod->name (scfg->axiom scfg)))

           )




