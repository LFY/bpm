(library (hash-cons)
         (export sexpr->dag
                 dag->sexpr
                 dag->sexpr-from

                 bimap-format
                 bimap->alist

                 bimap-size

                 dag-primitive?

                 )
         (import (except (rnrs) string-hash string-ci-hash)
                 (util)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)
                 )

         ;; Turning S-expressions into DAGS where intensionally-equal
         ;; subexpressions are only stored once

         ;; Reference: "Sharing and Common Subexpression Elimination in EDSL
         ;; compilation"
         ;; http://okmij.org/ftp/tagless-final/sharing/sharing.html

         ;; BiMap -- we use scheme hash table as the underlying implementation
         ;; see http://okmij.org/ftp/tagless-final/sharing/BiMap.hs

         (define (mk-bimap eq-fx)
           `(BiMap ,(make-hash-table eq-fx)
                   ,(make-hash-table eq-fx)))

         (define bimap->vks second)
         (define bimap->kvs third)

         ;; we encode Maybe type implicitly as functions taht return 'BiMap-None or the
         ;; answer we want

         (define Nothing 'BiMap-Nothing)
         (define (nothing? x) (eq? x Nothing))

         (define (bimap-lookup-key val bimap)
           (hash-table-ref (bimap->vks bimap) val (lambda () Nothing)))

         (define (bimap-lookup-val key bimap)
           (hash-table-ref (bimap->kvs bimap) key))

         ;; only reason to have bimap-insert return the new map is threading in the
         ;; state monad. the integer is what we actually care about calculating

         (define (bimap-insert val bimap)
           (let* ([vks (bimap->vks bimap)]
                  [kvs (bimap->kvs bimap)]
                  [new-idx (hash-table-size kvs)])
             (begin
               (hash-table-set! vks val new-idx)
               (hash-table-set! kvs new-idx val)
               new-idx)))

         ;; our goal: hashcons

         (define (hashcons node bimap)
           (let* ([key (bimap-lookup-key node bimap)])
             (cond [(nothing? key)
                    (bimap-insert node bimap)]
                   [else key])))

         ;; Our DAG node datatype directly corresponds to S-expressions.  Either an
         ;; atom, or a list with a symbol at the beginning and possibly some integers
         ;; that are references to other nodes. The idea is that each subexpression is
         ;; such a node, and we can turn any S-expression into a compressed
         ;; representation as a DAG.

         (define (mk-node name refs)
           (cons name refs))

         (define node->name car)
         (define node->refs cdr)

         (define (sexpr->dag e)
           (letrec* ([res-map (mk-bimap equal?)]
                     [loop (lambda (e)
                             (cond [(null? e) (hashcons '() res-map)]
                                   [(primitive? e) (hashcons e res-map)]
                                   [(list? e) (let* ([idxs (map loop (cdr e))]
                                                     [head (loop (car e))])
                                                ;; the thing here is, 
                                                (hashcons (cons (bimap-lookup-val head res-map) idxs) res-map))]))])
                    (begin (loop e)
                           res-map)))

         ;; We use the fact that the highest index of the hash table is the 'root'
         ;; element of the s-expression

         (define (dag->sexpr bimap) 
           (dag->sexpr-from (- (hash-table-size (bimap->kvs bimap)) 1) bimap))

         (define (dag->sexpr-from idx bimap)
           (letrec* ([kvs (bimap->kvs bimap)]
                     [root-node (hash-table-ref kvs idx)]
                     [loop (lambda (node)
                             (cond [(null? node) '()]
                                   [(primitive? node) node] ;; Primitive IFF has no refs
                                   [else (let* ([refs (node->refs node)]
                                                [other-nodes (map (lambda (r) (loop (hash-table-ref kvs r))) refs)])
                                           (cons (loop (car node)) other-nodes))]))])

                    (loop root-node)))


         (define (bimap-format bimap)
           (hash-table->alist (bimap->kvs bimap)))

         (define (bimap-size bimap)
           (hash-table-size (bimap->kvs bimap)))

         (define (bimap->alist bimap)
           (hash-table->alist (bimap->kvs bimap)))

         (define (dag-primitive? idx bimap)
           (primitive? (hash-table-ref (bimap->kvs bimap) idx)))


         ;; Tests
         )
