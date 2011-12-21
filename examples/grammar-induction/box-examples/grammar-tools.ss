(import (_srfi :1)
        (grammars)
        (util)
        (printing)
        (delimcc-simple-ikarus)
        (grammar-induction)
        (grammar-likelihood)
        (grammar-derivations-spread)
        (program))

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
        [(number? v) (number->string v)]
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

(define (nt-def->tex name-env show-param? choice)
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
                                                (primitive->tex tr)
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
                                                 (primitive->tex tr)
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

(define (latex-for f xs)
  (apply begin-latex (map f xs)))

(define (grammar->name-env gr)
  (map abstraction->name (program->abstractions gr)))

(define (latex-mergediff g1 g2 diff)
  (let* ([env1 (grammar->name-env g1)]
         [env2 (grammar->name-env g2)]
         [db (pretty-print diff)]
         [num (cadr diff)]
         [info (caddr diff)]
         [nts-before (car info)]
         [nts-after (caddr info)]
         [delta-stats (cdr (cadr (cadddr info)))])
    (begin-latex
      (textt (format "Merge: ~s to ~s" (car num) (cadr num))) latex-cr
      (latex-for (curry latex-nt env1) nts-before)
      Rightarrow quad
      (latex-for (curry latex-nt env2) nts-after)
      (latex-for
        (lambda (name-nums)
          (let* ([name (car name-nums)]
                 [nums (cdr name-nums)])
            (begin-latex
              (primitive->tex name) latex-align ":" (latex-for primitive->tex nums) latex-cr
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

(define (get-generalizations-from g1 g2)
  (let* ([models2 (map cadr (summarize g2))]
         [models1 (map cadr (summarize g1))]
         [m2-outside (filter (lambda (model) (out-of-scope? model g1)) models2)])
    m2-outside))

(define (summary->graffles summary-idx grammar-summary)
  (let* ([cleaned-summary (delete-duplicates (cadr grammar-summary))]
         [graffles 
           (map (lambda (model-idx prob-model)
                  (let* (
                         [prob (car prob-model)]
                         [graffle-name (string-append 
                                         prefix
                                         "_" 
                                         (number->string summary-idx) 
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

