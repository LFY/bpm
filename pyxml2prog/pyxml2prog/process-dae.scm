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
(define keep (lambda x x))

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


(define (delete-all-attrs attrs) '())

(define (keep-attrs names)
  (lambda (attrs)
    (my-filter (lambda (a) (kv-member-of a names)) attrs)))

(define (simplify-geo tag attrs . children)
  `(,tag ,attrs ,@(map (lambda (elt)
                         (pre-post-order elt
                                         `(
                                           (dae:bind_material . ,kill)
                                           (*default* *preorder* . ,keep)
                                           (*text* . ,(lambda (tag str) str)))))
                       children)))

(define (simplify-node tag attrs . children)
  (let* (
         [geo-element ([sxpath '()] children)]
         ;;[geo-element (car children)]
         )
    `(,tag ,attrs ,@(map (lambda (elt)
                           
                           (pre-post-order elt 
                                           `(
                                             (dae:instance_geometry . ,simplify-geo)
                                             (dae:matrix . ,keep)
                                             (dae:node . ,simplify-node)
                                             (dae:extra . ,kill)

                                             (*default* *preorder* . ,keep)
                                             (*text* . ,(lambda (tag str) str)))))
                         children))))

(define (simplify-dae dae-elements)
  (map (lambda (elt)
         (pre-post-order elt
                         `(

                           (dae:library_visual_scenes . ,keep)
                           (dae:visual_scene . ,keep)
                           (dae:node . ,simplify-node)
                           ;; (dae:matrix . ,keep)
                           ;; (dae:instance_geometry . ,keep)

                           ;; (dae:extra . ,kill)
                           ;; (dae:bind_material . ,kill)

                           ; Default
                           (*default* *preorder* . ,keep)
                           (*text* . ,(lambda (tag str) str)))))
       dae-elements))

(define (words str)
  (define (loop acc word xs)
    (cond [(null? xs) (cond [(null? word) acc]
                            [else (cons word acc)])]
          [(eq? #\space (car xs))
           (cond [(null? word) (loop acc (cdr xs))]
                 [else (loop (cons word acc) '() [cdr xs])])]
          [else (loop acc (cons (car xs) word) (cdr xs))]))
  (let* ([xs (string->list str)])
    (map (lambda (xs) (list->string (reverse xs))) (loop '() '() xs))))

(define (cleanup-url url)
  (let* ([str (cadr url)]
         [new-str (substring str 1 (string-length str))])
    `(url ,new-str)))

;; Tables for keeping track of original Collada elements.

(define (make-equal-hashtable) (make-hashtable equal-hash equal?))

(define elt-sym->elt-table (make-equal-hashtable))
(define elt-hash->sym-table (make-equal-hashtable))

(define tr-sym->tr-table (make-equal-hashtable))
(define tr-hash->sym-table (make-equal-hashtable))

(define elt-sym-count 0)
(define (new-elt-sym)
  (let* ([answer (string->symbol (string-append "elt" (number->string elt-sym-count)))])
    (begin (set! elt-sym-count (+ 1 elt-sym-count))
           answer)))

(define tr-sym-count 0)
(define (new-tr-sym)
  (let* ([answer (string->symbol (string-append "trans" (number->string tr-sym-count)))])
    (begin (set! tr-sym-count (+ 1 tr-sym-count))
           answer)))

(define (hash-table-ref table key thunk)
  (if (hashtable-contains? table key)
    (hashtable-ref table key 'ERROR)
    (let* ([new-val (thunk)])
      new-val)))

;; context: elt with a child that is a dae:instance_geometry
(define (elt-hash->sym elt)
  (let* ([geom-elt (car ([sxpath '(dae:instance_geometry)] elt))]
         [hash-val (cadr (car ([sxpath '(@ url)] geom-elt)))]
         ;; [sym (if (hashtable-contains? elt-hash->sym-table hash-val)
         ;;        (hashtable-ref elt-hash->sym-table hash-val 'ERROR))
         ;;        (let* ([new-sym (new-elt-sym)])
         ;;                       (begin (hashtable-set! elt-hash->sym-table hash-val new-sym)
         ;;                              new-sym)))])
         [sym (hash-table-ref elt-hash->sym-table hash-val 
                             (lambda () (let* ([new-sym (new-elt-sym)])
                               (begin (hashtable-set! elt-hash->sym-table hash-val new-sym)
                                      new-sym))))])
    (begin (hash-table-ref elt-sym->elt-table sym 
                          (lambda () (begin (hashtable-set! elt-sym->elt-table sym geom-elt)
                                            geom-elt)))
           sym)))

(define (iota n)
  (define (loop acc n)
    (cond [(= 0 n) (cons n acc)]
          [else (loop (cons n acc) (- n 1))]))
  (loop '() (- n 1)))

(define (bin-transform model-scale entries)
  (define (bin-by x v)
    (inexact->exact (floor (/ v x))))
  (let* ([indices (iota (length entries))]

         ;; We would like to keep the "administrative" entries of the matrix
         ;; (the rotation/scaling factors), it's the translations that need to
         ;; be binned.

         [non-administrative? (lambda (i) (= 3 (mod i 4)))])
    (map (lambda (idx val) (cond [(non-administrative? idx) (bin-by model-scale val)]
                                 [else val]))
         indices entries)))

;; context: elt with a child that is a dae:matrix
(define (tr-hash->sym model-scale elt)
  (let* ([tr-elt (car ([sxpath '(dae:matrix)] elt))]
         [hash-val (bin-transform model-scale (map string->number (reverse (words (car ([sxpath '(*text*)] tr-elt))))))]
         ;;[hash-val 0];; (bin-transform model-scale (map string->number (words (car ([sxpath '(*text*)] tr-elt)))))]
         [sym (hash-table-ref tr-hash->sym-table hash-val 
                             (lambda () (let* ([new-sym (new-tr-sym)])
                               (begin (hashtable-set! tr-hash->sym-table hash-val new-sym)
                                      new-sym))))])
    (begin (hash-table-ref tr-sym->tr-table sym 
                          (lambda () (begin (hashtable-set! tr-sym->tr-table sym tr-elt)
                                            tr-elt)))
           sym)))

(define (hash-table->alist table)
  (let* ([keys (hashtable-keys table)])
    (vector->list (vector-map (lambda (k) (list k (hashtable-ref table k 'ERROR))) keys))))

(define (rearrange-dae model-scale elt)
  (begin 
    (cond
           [(null? elt) '()] 
           [(eq? 'dae:node (car elt))
            (begin 
              ;; (pp "equal to node")
                   ;; (pp elt)
                   (cond [(null? ([sxpath '(dae:instance_geometry)] elt)) '()]
                         [else
                           (let* ([geo-elt (symbol->string (elt-hash->sym elt))]
                                  ;; [db (pp "got geo elt")]
                                  [children-nodes (my-filter (lambda (n) (eq? (car n) 'dae:node)) 
                                                             (cddr elt))]
                                  ;; [db (pp "got children")]
                                  [children-rearranged (map (lambda (n)
                                                              `(tr ,(symbol->string (tr-hash->sym model-scale n))
                                                                   ,(rearrange-dae model-scale n)))
                                                            children-nodes)])
                             `(elem ,geo-elt ,@children-rearranged))]))])))

(define (postprocess tree)
  (pre-post-order tree
                  `(
                    (*default* . ,(lambda (tag . elts)
                                    `(,tag ,@(my-filter (lambda (x) (not (null? x))) elts))))
                    (*text* . ,(lambda (tag str) str))
                    )))

(define (process-dae model-scale port)
  (let* (
         ;; [db (pp "reading collada file")]
         [doc (ssax:xml->sxml port '[(dae . "http://www.collada.org/2005/11/COLLADASchema")])]
         ;; [db (pp "selecting visual scenes")]
         [doc2 ([sxpath '(dae:COLLADA dae:library_visual_scenes dae:visual_scene dae:node)] doc)]
         ;; [db (pp "rearranging..")]
         [result (my-filter (lambda (n) (not (null? n))) (map (lambda (elt) (rearrange-dae model-scale elt)) doc2))];; (postprocess (simplify-dae doc2)))]
         ;; [db (pp "done rearranging")]
         )
    (begin
      ;; (pp result)
      ;; (pp (hash-table->alist elt-sym->elt-table))
      ;; (pp (hash-table->alist tr-sym->tr-table))
      (pp `(num-transforms ,(length (hash-table->alist tr-sym->tr-table))))
      (list result 
            (hash-table->alist elt-sym->elt-table)
            (hash-table->alist tr-sym->tr-table)))))

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

(define (data->scheme-experiment orig-fn xml elt-table tr-table weight-params)
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

  (let* ([stop-number (list-ref weight-params 0)]
         [beam-width (list-ref weight-params 1)]
         [likelihood-weight (list-ref weight-params 2)]
         [prior-weight (list-ref weight-params 3)]
         [prior-parameter (list-ref weight-params 4)]
         [num-threads (list-ref weight-params 5)]
         [model-spacing (list-ref weight-params 6)]
         [num-models (list-ref weight-params 7)]
         [reconstitute (list-ref weight-params 8)]
         [param-string (list-ref weight-params 9)]
         [db (pp param-string)])
    `((import (rnrs) (_srfi :1) (grammar-induction) (scene-graphs) (printing))
      (define test-data (quote ,xml))
      (define elements (quote ,elt-table))
      (define transforms (quote ,tr-table))
      (pretty-print test-data)
      (define output-grammar (gi-bmm test-data 
                                     ,stop-number
                                     ,beam-width
                                     ,likelihood-weight 
                                     ,prior-weight
                                     ,prior-parameter
                                     ,num-threads))
      (print "Resulting grammar:")
      (pretty-print output-grammar)
      (system (format "rm ~s" ,(string-append orig-fn param-string ".grammar.ss")))
      (output-scene-sampler ,orig-fn
                            ,(string-append orig-fn param-string ".grammar.ss") 
                            output-grammar 
                            elements 
                            transforms 
                            ,(string-append orig-fn param-string ".scene")
                            ,model-spacing
                            ,num-models
                            ,reconstitute))))

(define (main argv)

  (let* ([dae-filename (list-ref argv 1)]
         [ss-filename (list-ref argv 2)]
         [model-scale (string->number (list-ref argv 3))]
         [stop-number (list-ref argv 4)]
         [beam-size (list-ref argv 5)]
         [likelihood-weight (list-ref argv 6)]
         [prior-weight (list-ref argv 7)]
         [prior-parameter (list-ref argv 8)]
         [num-threads (list-ref argv 9)]
         [model-spacing (list-ref argv 10)]
         [num-models (list-ref argv 11)]
         [reconstitute (list-ref argv 12)]
         [param-string (list-ref argv 13)]
         [processed-data 
           (call-with-input-file 
             dae-filename 
             (lambda (port) (process-dae model-scale port)))]
         [data-examples (car processed-data)]
         [element-table (cadr processed-data)]
         [transform-table (caddr processed-data)]
         )
    (with-output-to-file (caddr argv)
                         (lambda () 
                           (for-each pp
                                     (data->scheme-experiment (cadr argv) data-examples element-table transform-table 
                                                              (append
                                                                (map string->number 
                                                                     (list stop-number
                                                                           beam-size
                                                                           likelihood-weight
                                                                           prior-weight
                                                                           prior-parameter
                                                                           num-threads
                                                                           model-spacing
                                                                           num-models
                                                                           reconstitute))
                                                                (list param-string))
                                                              )))
                         'replace)))

