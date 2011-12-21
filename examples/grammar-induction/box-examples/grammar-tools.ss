(import (_srfi :1)
        (grammars)
        (util)
        (printing)
        (delimcc-simple-ikarus)
        (grammar-induction)
        (grammar-likelihood)
        (grammar-derivations-spread)
        (write-boxes)
        (program)
        (_srfi :69))

(define (mk-elem type trans xs)
  (cond [(null? xs) `(elem ,type)]
        [else `(elem ,type (tr ,trans ,@xs))])) 

(define (mk-elem-2 type tr1 tr2 xs)
  (cond [(null? xs) `(elem ,type)]
        [else `(elem ,type 
                     (tr ,tr1 ,(car xs))
                     (tr ,tr2 ,(cadr xs)))]))

(define (auto-elem type xs)
  (cond [(null? xs) `(elem ,type)]
        [(= 1 (length xs)) (mk-elem type "forward" xs)]
        [(= 2 (length xs)) (mk-elem-2 type "l-forward" "r-forward" xs)]
        [else '()]))

(define (gray . xs) (auto-elem "gray" xs))

(define (red . xs) (auto-elem "red" xs))

(define (blue . xs) (auto-elem "blue" xs))

(define (root . xs) (mk-elem-2 "root" "left" "right" xs))

(define (chain elt-type n next-chain)
  (cond [(= 0 n) next-chain]
        [else `(elem ,elt-type 
                     (tr "forward" 
                         ,(chain elt-type (- n 1) next-chain)))]))

(define (alt-chain k n type1 type2 acc)
  (cond [(= 0 k) acc]
        [else
          (alt-chain (- k 1) n type2 type1 
                     (chain type1 (- n 1) acc))]))

(define merge-history '())


(define (numbered-nt? nt-name)
  (equal? "F" (substring (symbol->string nt-name) 0 1)))

;; Pretty-printing

(define max-places 2)

(define (trunc-float num)
  (let* ([whole-dec (str-split (number->string num) #\.)])
    (string-append (car whole-dec) "." (if (= 1 (length whole-dec)) "" (cond [(> (string-length (cadr whole-dec)) max-places) (substring (cadr whole-dec) 0 max-places)] [else (cadr whole-dec)])))))

(define (nt-name->sym+num nt-name)
  (let* ([name-str (symbol->string nt-name)]
         [len (string-length name-str)]
         [sym (string->symbol (substring name-str 0 1))]
         [num (string->number (substring name-str 1 len))])
    (list sym num)))

(define (nt-name->num nt-name)
  (let* ([name-str (symbol->string nt-name)]
         [len (string-length name-str)]
         [num (string->number (substring name-str 1 len))])
    num))

(define str->tex printf)

(define (textt x) (string-append "\\texttt{" x "}"))
(define (textup x) (string-append "\\textup{" x "}"))
(define (sub x y) (string-append x "_{" y "}"))
(define rightarrow "\\rightarrow ")
(define Rightarrow "\\Rightarrow ")
(define latex-align "&")
(define begin-latex string-append)
(define (brack . xs) (string-append "\\left[" (apply string-append xs) "\\right]"))
(define latex-cr "\\\\\n")
(define (latex-line . xs) (string-append (apply string-append xs) "\\\\\n"))
(define qquad "\\qquad ")
(define quad "\\quad ")

(define (latex-cmd name . args)
  (begin-latex
    "\\" name
    (delimit "" (map (lambda (arg)
           (string-append "{" arg "}"))
         args))))

(define (primitive->tex v)
  (cond [(symbol? v) (string-append (textt (symbol->string v)) "\\,")]
        [(string? v) (string-append (textt v) "\\,")]
        [(number? v) (trunc-float v)]
        [(list? v) (list->tex v)]))

(define (nt-name->tex nt-name)
  (cond
    [(equal? "F" (substring (symbol->string nt-name) 0 1))
     (let* ([sym+num (nt-name->sym+num nt-name)])
       (begin-latex
         (sub
           (symbol->string (car sym+num))
           (number->string (cadr sym+num)))
         "\\,"
         ))]
    [else (begin-latex (primitive->tex (symbol->string nt-name)) "\\,")]))

(define (has-param? choice)
  (number? (car choice)))

(define tr-table (make-hash-table equal?))
(define tr-counter 0)
(define (nt-def->tex name-env show-param? choice)

  (define (simple-tr tr)
    (begin-latex "T_{" (number->string
                         (hash-table-ref tr-table tr
                                       (lambda ()
                                         (begin
                                           (hash-table-set! tr-table tr tr-counter)
                                           (set! tr-counter (+ 1 tr-counter))
                                           tr-counter)))) "}"))

  (cond [(has-param? choice)
         (let* ([param (car choice)]
                [body (cadr choice)])
           (cond [(equal? 'elem (car body))
                  (let* ([elem (cadr body)]
                         [children (cddr body)])
                    (begin-latex
                      (if show-param? (begin-latex (primitive->tex param) ":") "")
                      (primitive->tex elem)
                      (cond [(null? children) ""]
                            [else 
                              (apply begin-latex 
                                     (map (lambda (child)
                                            (let* ([tr (cadr child)]
                                                   [succ (caaddr child)])
                                              (brack
                                                (simple-tr tr)
                                                (nt-name->tex succ))))
                                          children))])))]
                 [else 
                   (let* ([succ (car body)])
                     (begin-latex
                       (if show-param? (begin-latex (primitive->tex param) ":") "")
                       (nt-name->tex succ)))]))]
        [else
          (let* ([body choice]
                 [param 1.0])
            (cond [(equal? 'elem (car body))
                   (let* ([elem (cadr body)]
                          [children (cddr body)])
                     (begin-latex
                       (if show-param? (begin-latex (primitive->tex param) ":") "")
                       (primitive->tex elem)
                       (cond [(null? children) ""]
                             [else 
                               (apply begin-latex 
                                      (map (lambda (child)
                                             (let* ([tr (cadr child)]
                                                    [succ (caaddr child)])
                                               (brack
                                                 (simple-tr tr)
                                                 (nt-name->tex succ))))
                                           children))])))]
                  [else 
                    (let* ([succ (car body)])
                      (begin-latex
                        (if show-param? (begin-latex (primitive->tex param) ":") "")
                        (nt-name->tex succ)))]))] 
          ))

(define (latex-nt name-env nt)
  (define (process-choices latex-defs)
    (begin-latex (foldr1
      (lambda (def1 def2)
        (begin-latex
          def1 "|" latex-cr
          def2))
      (map (lambda (def)
             (begin-latex "&" def)) latex-defs)) latex-cr))
  (let* ([latex-name (nt-name->tex (nt->name nt))]
         [choices (nt->choices nt)]
         ;;[show-param? (not (= 1 (length choices)))]
         [show-param? #f]
         [latex-defs (map (curry nt-def->tex name-env show-param?) (nt->choices nt))])
    (begin-latex
      latex-name 
      rightarrow quad
      (process-choices latex-defs))))

(define
  (latex-grammar grammar)
  (let* ([tied-params (grammar->tied-params2 grammar)]
         [nts (program->abstractions tied-params)]
         [name-env (map abstraction->name nts)])
    (for-each str->tex
              (map (curry latex-nt name-env) nts))))


(define
  (latex-grammar-nts grammar)
  (let* ([tied-params (grammar->tied-params2 grammar)]
         [nts (program->abstractions tied-params)]
         [name-env (map abstraction->name nts)])
    (apply begin-latex
              (map (curry latex-nt name-env) nts))))


(define (latex-eqn-table . cols)
  (begin-latex
    "\\begin{tabular}{" (delimit "" (map (lambda (i) "c") cols)) "}"
    (delimit "&" (map (lambda (col) (begin-latex "$\\begin{aligned}" col "\\end{aligned}$")) cols))
    "\\end{tabular}"))

(define (latex-for f xs)
  (apply begin-latex (map f xs)))

(define (grammar->name-env gr)
  (map abstraction->name (program->abstractions gr)))

(define Delta "\\Delta ")
(define given "|")
(define (paren . xs) (begin-latex "\\left(" (delimit " " xs) "\\right)"))
(define (P x y z) (begin-latex "P" (paren x y z)))
(define (L x y z) (begin-latex "L" (paren x y z)))
(define (pri-dist x) (begin-latex "\\pi" (paren x)))
(define Grammar "\\mathbf{G}")
(define Models "\\mathbf{M}")
(define Structure "S")
(define theta "\\theta ")

(define (lookup-latex-name sym)
  (cadr (assoc sym
               `(
                 (d-posterior ,(begin-latex Delta (P Grammar given Models)))
                 (d-likelihood ,(begin-latex Delta (L Models given Grammar)))
                 (d-prior ,(begin-latex Delta (pri-dist Grammar)))
                 (d-dl ,(begin-latex Delta "DL"))

                 (posterior ,(begin-latex (P Grammar given Models)))
                 (likelihood+weight ,(begin-latex (L Models given Grammar)))
                 (prior+weight ,(begin-latex (pri-dist Grammar)))
                 (desc-length ,(begin-latex "DL"))
                 (dirichlet-prior ,(begin-latex (sub "P" Grammar) (paren (sub theta Grammar) given (sub Structure Grammar))))
                 
                 ))))

(define (latex-grammar-stats grammar)
  (let* ([stats (grammar->stat-vec grammar)])
    (latex-for
      (lambda (name-nums)
        (let* ([name (car name-nums)]
               [nums (cdr name-nums)])
          (begin-latex
            (lookup-latex-name name) latex-align ":" (latex-for primitive->tex (list (car nums))) latex-cr
            )))
      stats)))

(define (latex-grammar-full grammar)
  (latex-eqn-table
    (latex-grammar-nts grammar)
    (latex-grammar-stats grammar)))

(define (latex-mergediff-horiz g1 g2 diff)
  (let* ([env1 (grammar->name-env g1)]
         [env2 (grammar->name-env g2)]
         [db (pretty-print diff)]
         [num (cadr diff)]
         [info (caddr diff)]
         [nts-before (car info)]
         [nt-names-before (map nt->name nts-before)]
         [nts-after (caddr info)]
         [nt-names-after (map nt->name nts-after)]
         [new-nt (car (filter (lambda (nt) (not (contains? (nt->name nt) nt-names-before))) nts-after))]
         [merged-nts (filter  (lambda (nt) (not (contains? (nt->name nt) nt-names-after))) nts-before)]
         [delta-stats (cdr (cadr (cadddr info)))])
    (latex-eqn-table
      (latex-for (curry latex-nt env1) merged-nts)
      (begin-latex Rightarrow quad (latex-nt env2 new-nt))
      (latex-for
        (lambda (name-nums)
          (let* ([name (car name-nums)]
                 [nums (cdr name-nums)])
            (begin-latex
              (lookup-latex-name name) latex-align ":" (latex-for primitive->tex nums) latex-cr
              )))
        delta-stats)
      )))

(define (latex-mergediff-all g1 g2 diff)
  (let* ([env1 (grammar->name-env g1)]
         [env2 (grammar->name-env g2)]
         [db (pretty-print diff)]
         [num (cadr diff)]
         [info (caddr diff)]
         [nts-before (car info)]
         [nt-names-before (map nt->name nts-before)]
         [nts-after (caddr info)]
         [nt-names-after (map nt->name nts-after)]
         [new-nt (car (filter (lambda (nt) (not (contains? (nt->name nt) nt-names-before))) nts-after))]
         [merged-nts (filter  (lambda (nt) (not (contains? (nt->name nt) nt-names-after))) nts-before)]
         [delta-stats (cdr (cadr (cadddr info)))])
    (latex-eqn-table
      (latex-for (curry latex-nt env1) nts-before)
      (begin-latex Rightarrow quad (latex-for (curry latex-nt env1) nts-after))
      (latex-for
        (lambda (name-nums)
          (let* ([name (car name-nums)]
                 [nums (cdr name-nums)])
            (begin-latex
              (lookup-latex-name name) latex-align ":" (latex-for primitive->tex nums) latex-cr
              )))
        delta-stats)
      )))

(define (latex-mergediff g1 g2 diff)
  (let* ([env1 (grammar->name-env g1)]
         [env2 (grammar->name-env g2)]
         [db (pretty-print diff)]
         [num (cadr diff)]
         [info (caddr diff)]
         [nts-before (car info)]
         [nt-names-before (map nt->name nts-before)]
         [nts-after (caddr info)]
         [nt-names-after (map nt->name nts-after)]
         [new-nt (car (filter (lambda (nt) (not (contains? (nt->name nt) nt-names-before))) nts-after))]
         [merged-nts (filter  (lambda (nt) (not (contains? (nt->name nt) nt-names-after))) nts-before)]
         [delta-stats (cdr (cadr (cadddr info)))])
    (begin-latex
      (textt (format "Merge: ~s to ~s" (car num) (cadr num))) latex-cr
      (latex-for (curry latex-nt env1) merged-nts)
      Rightarrow quad
      (latex-nt env2 new-nt)
      (latex-for
        (lambda (name-nums)
          (let* ([name (car name-nums)]
                 [nums (cdr name-nums)])
            (begin-latex
              (lookup-latex-name name) latex-align ":" (latex-for primitive->tex nums) latex-cr
              )))
        delta-stats)
      )))

(define (pp-mergediff diff)
  (let* ([num (cadr diff)]
         [info (caddr diff)]
         [nts-before (car info)]
         [nts-after (caddr info)]
         [delta-stats (cdr (cadr (cadddr info)))])
    (begin
      (print "Merge: ~s to ~s" (car num) (cadr num))
      (for-each pretty-print nts-before)
      (print "===>")
      (for-each pretty-print nts-after)
      (for-each
        (lambda (name-nums)
          (let* ([name (car name-nums)]
                 [nums (cdr name-nums)])
            (begin
              (display
                (apply (curry string-append (symbol->string name)
                               ": ") (map number->string nums))) (display " ")
              )))
        delta-stats)
      (newline))))

;; Summarization

(define (summarize g) (grammar-derivations g 0.01))

(define (likelihood-of-model model gr) (single-data-grammar->likelihood (list model) gr))
(define (out-of-scope? model gr) (= -inf.0 (likelihood-of-model model gr)))

(define (get-new-models g2 g1)
  (let* ([models2 (map cadr summ2)]
         [models1 (map cadr summ1)]
         [m1-not-m2 (lset-difference equal? models1 models2)]
         [m2-not-m1 (lset-difference equal? models2 models1)])
    m2-not-m1))

(define-opt (get-generalizations-from g1 g2 (optional (eps 0.01)))
            (let* ([prob-models2 (grammar-derivations g2 eps)]
                   [prob-models1 (grammar-derivations g1 eps)]
                   [m2-outside (filter (lambda (prob-model) (out-of-scope? (cadr prob-model) g1)) prob-models2)])
              m2-outside))

(define (summary->graffles prefix summary)
  (let* ([db (pretty-print summary)]
         [cleaned-summary (delete-duplicates summary)]
         [graffles 
           (map (lambda (model-idx prob-model)
                  (let* (
                         [prob (car prob-model)]
                         [graffle-name (string-append 
                                         prefix
                                         "_" 
                                         (number->string model-idx)
                                         "_"
                                         (number->string prob) 
                                         ".sxml")]
                         [scene (cadr prob-model)]
                         [graffle-scene (box-scene->graffle scene)]
                         )
                    `(graffle 
                       ,graffle-name
                       ,graffle-scene)))
                (indices cleaned-summary) cleaned-summary)])
    graffles))

(define (write-graffle graffle)
  (let* ([filename (cadr graffle)]
         [sxml (caddr graffle)])
    (begin
      (system (format "rm ~s" filename))
      (with-output-to-file filename
                           (lambda ()
                             (pretty-print sxml)))
      (system (string-append "python reconst-graffle.py " filename " " (string-append filename ".graffle"))))))

