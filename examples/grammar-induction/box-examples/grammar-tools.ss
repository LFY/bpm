(import (_srfi :1)
        (grammars)
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

