(import (_srfi :1))
(define (root x y)
  `(elem "root"
         (tr "left" ,x)
         (tr "right" ,y)))

(define (mk-elem type trans . xs)
  (cond [(null? xs) `(elem ,type)]
        [else `(elem ,type (tr ,trans ,@xs))]))

(define (gray . xs)
  (apply (curry mk-elem "gray" "forward") xs))

(define (red . xs)
  (apply (curry mk-elem "red" "forward") xs))

(define (blue . xs)
  (apply (curry mk-elem "blue" "forward") xs))

(define data
  (list
    (root (red (red (red)))
          (red (red (red))))
    (root (red (red))
          (red (red)))
    (root (red) (red))
    (root (red (red (gray (blue (gray)))))
          (red (red (gray (blue (gray))))))
    (root (red (red (gray)))
          (red (red (gray))))
    ))

(define merge-history '())
