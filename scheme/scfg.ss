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
           )




         )
