; Based on original file in SSAX/lib/examples:
; $Id: run-sxml.scm,v 1.2 2002/12/10 22:28:14 oleg Exp $

(define docstrings
 '(
 " Transform an XML document into SXML. See ../docs/SXML.html for description."
 ""
 " Usage"
 "	xml-to-sxml xml-file-name"
 " The translated SXML document is printed to the output file specified."
))

; The XML->SXML function is a part of the SSAX distribution

(define (parser-error port message . specialising-msgs)
  (apply cerr (cons message specialising-msgs))
  (cerr nl)
  (exit 4))
(define (ssax:warn port message . specialising-msgs)
  (apply cerr (cons message specialising-msgs))
  (cerr nl))

(define (contains? x xs)
  (not (eq? #f (member x xs))))

(define (my-filter pred lst)
  (cond
    ((null? lst) lst)
    ((pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))))
    (else (my-filter pred (cdr lst)))))

(define (tag-map tag)
  (cond [(eq? 'svg:svg tag) 'my-svg]
        [(eq? 'svg:g tag) 'my-group]
        [(eq? 'svg:rect tag) 'my-rect]))

(define (test-attr-fx attrs) attrs)

(define (test-kill-pred elt)
  (let* ([tag (car elt)])
    (contains? tag '(svg:defs svg:metadata))))


(define kill (lambda _ '()))

(define (kv-member-of kv keys)
  (contains? (car kv) keys))

(define (replace-tag-filter-attrs rep-tag attr-pred loop)
  (lambda (tag attrs . elements)
    (let ([db (begin
                (display "begin replace-tag-filter-attrs\n")
                (display (cdr attrs)) (display "\n")
                (display (length attrs)) 
                (display "\n"))]
          [remaining-attrs (my-filter attr-pred (cdr attrs))])
      `(,rep-tag ,(cons '@ remaining-attrs) ,@(loop elements)))))

(define (replace-tag-transform-attrs rep-tag attr-fx loop)
  (lambda (tag attrs . elements)
    (let ([transformed-attrs (attr-fx (cdr attrs))])
      `(,rep-tag ,(cons 'data transformed-attrs) ,@(loop elements)))))

(define (postprocess tree)
  (pre-post-order tree
                  `(
                    (*default* . ,(lambda (tag . elts)
                                    `(,tag ,@(my-filter (lambda (x) (not (null? x))) elts))))
                    (*text* . ,(lambda (tag str) str))
                    )))

(define (delete-all-attrs attrs) '())

(define (keep-attrs names)
  (lambda (attrs)
    (my-filter (lambda (a) (kv-member-of a names)) attrs)))


(define (simplify-bento bento-elements)

  (define (reformat-number kv)
    (let* ([k (car kv)]
           [v (cadr kv)])
      (cond [(contains? k '(x y width height)) `(,k ,(string->number v))]
            [else kv])))

  (define (keep-and-reformat names)
    (lambda (attrs)
      (map reformat-number ((keep-attrs names) attrs))))

  (define measurement-fields '(x y width height docWidth docHeight))

  (define (pull-out-constructors xs)
    (define my-nil 'NIL)
    (define (my-cons x y) (if (null? x) 
                            `(,x ,y)
                            `(node (data) ,x ,y)))
    (if (null? xs) '()
      (my-cons (car xs) (pull-out-constructors (cdr xs)))))


  (define (clean elt)
    (replace-tag-transform-attrs elt (keep-and-reformat measurement-fields)
                                 ;; (lambda (children) (pull-out-constructors children))
                                 (lambda (children) children)))
                                 
                                 ;; pull-out-constructors))
  (define (kill-attrs elt)
    (replace-tag-transform-attrs 'killed-block delete-all-attrs simplify-bento))

  (define (add-empty-attr)
    (lambda (tag . children)
      `(node (data) ,@children)))

  (map (lambda (elt)
         (pre-post-order elt
                         `(
                           (page . ,(lambda x x))
                           (bentoTree . ,(add-empty-attr))
                           (bentoBlock . ,(clean 'node))
                           (*default* *preorder* . ,(lambda x x))

                           (featureData . ,kill)

                           ; Default
                           (*text* . ,(lambda (tag str) str)))))
       bento-elements))

(define (process-bento port)
  (let* ([doc (ssax:xml->sxml port '())]
         [doc2 ([sxpath '(response page page)] doc)]
         [db (pp doc2)]
         [result (car (postprocess (simplify-bento ([sxpath '(response page page bentoTree)] doc))))]
         )
    (begin
      (pp result)
      result)))

(define (fold acc z xs)
  (if 
    (null? xs) z
    (fold acc (acc (car xs) z) (cdr xs))))

(define (concatenate xs)
  (fold (lambda (x y) (append x y)) '() xs))

(define (delete-duplicates xs)
  (define (loop acc xs)
    (cond [(null? xs) acc]
          [(contains? (car xs) acc) (loop acc (cdr xs))]
          [else (loop (cons (car xs) acc) (cdr xs))]))
  (loop '() xs))

(define (data->scheme-experiment xml)
  (define (mk-defines-from-tags xml)
    (define tags '())
    (define (loop acc xml)
      (cond [(null? xml) acc]
            [else (let* ([tag (car xml)]
                         [effect (begin
                                   (set! tags (if (contains? tag tags) tags (cons tag tags))))]
                         [children (cdr (cdr xml))]
                         [new-acc (cons tag acc)])
                    (concatenate (map (lambda (c) (loop new-acc c)) children)))]))
    (begin
      (loop '() xml)
      (map (lambda (t) `(define ,t node)) tags)))

  `((import (rnrs) (_srfi :1) (beam-learning) (printing))
    (define test-data (list (quote ,xml)))
    (pretty-print test-data)
    (pretty-print (learn-model test-data 1 10))))

(define (main argv)

  (define (help)
    (for-each
     (lambda (docstring) (cerr docstring nl))
     docstrings)
    (exit 4))

  (if (not (= 3 (length argv)))
      (help))		; at least one argument, besides argv[0], is expected

  (let* ([processed-data (call-with-input-file (cadr argv) process-bento)])
    (with-output-to-file (caddr argv)
                         (lambda () 
                           (for-each pp
                                     (data->scheme-experiment processed-data)))
                         'replace))
)

