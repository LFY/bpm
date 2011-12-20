(import (_srfi :1))
;; A root node, 0 gray nodes, and a chain of red/blue on the left and right.

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

(define (alternating-chain n)
  `(elem "root"
         (tr "left" ,(alt-chain n 3 "red" "blue" '(elem "gray")))
         (tr "left" ,(alt-chain n 3 "red" "blue" '(elem "gray")))))

(define (root x y)
  `(elem "root"
         (tr "left" ,x)
         (tr "right" ,y)))

(define (gray . xs)
  `(elem "gray" (tr "forward" ,@xs)))

(define (red . xs)
  `(elem "red" (tr "forward" ,@xs)))

(define (blue . xs)
  `(elem "blue" (tr "forward" ,@xs)))


(define data
  (list
    (root (gray (blue))
          (gray (red (red))))
    (root (gray (blue))
          (gray (red (red (gray (blue))))))
    (root (gray (blue (gray (blue (gray (blue))))))
          (gray (red (red (gray (red (red (gray (red (red))))))))))
    ))


(define merge-history '())

