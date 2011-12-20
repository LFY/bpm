(import (_srfi :1)
        (grammars)
        (util)
        (printing)
        (delimcc-simple-ikarus)
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

(define (tie-parameters grammar+params)
  (define (grammar->params grammar+params)
    (cadddr grammar+params))
  (define (my-grammar->nts grammar+params)
    (append (program->abstractions grammar+params)
            (list `(abstraction Start () ,(caddr (program->body grammar+params))))))
  (define nts-with-params
    (map (lambda (nt params)
           (let* ([choices (cond [(eq? 'choose (car (abstraction->pattern nt))) 
                                  (cdr (abstraction->pattern nt))]
                                 [else (list (abstraction->pattern nt))])]
                  )
             `(abstraction
                ,(abstraction->name nt)
                ()
                (choose 
                  ,@(map 
                      (lambda (param choice) (list (exp param) choice)) 
                      params choices)))))
         (my-grammar->nts grammar+params) (grammar->params grammar+params)))
  (grammar-with-new-nts+body grammar+params nts-with-params '(lambda () (Start))))

(define (nt-name->sym+num nt-name)
  (let* ([name-str (symbol->string nt-name)]
         [len (string-length name-str)]
         [sym (string->symbol (substring name-str 0 1))]
         [num (string->number (substring name-str 1 len))])
    (list sym num)))

(define str->tex printf)

(define (textt x) (string-append "\\texttt{" x "}"))
(define (sub x y) (string-append x "_{" y "}"))
(define rightarrow "\\rightarrow")
(define latex-align "&")
(define begin-latex string-append)
(define (brack . xs) (string-append "\\left[" (apply string-append xs) "\\right]"))
(define latex-cr "\\\\\n")
(define (latex-line . xs) (string-append (apply string-append xs) "\\\\\n"))
(define qquad "\\qquad")
(define quad "\\quad")

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

(define (nt-def->tex name-env choice)
  (let* ([param (car choice)]
         [body (cadr choice)])

    (cond [(equal? 'elem (car body))
           (let* ( [elem (cadr body)]
                  [children (cddr body)])
             (begin-latex
               (primitive->tex param)
               ":"
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
                (primitive->tex param)
                ":"
                (nt-name->tex succ)))])))


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
         [latex-defs (map (curry nt-def->tex name-env) (nt->choices nt))])
    (begin-latex
      latex-name 
      rightarrow quad
      (process-choices latex-defs))))

(define
  (latex-grammar grammar)
  (let* ([tied-params (tie-parameters grammar)]
         [nts (program->abstractions tied-params)]
         [name-env (map abstraction->name nts)])
    (for-each str->tex
              (map (curry latex-nt name-env) nts))))

