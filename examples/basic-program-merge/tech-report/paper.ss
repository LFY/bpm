(import (bayes-program-merging)
        (util)
        (_srfi :1)
        (node-constructors)
        )


(define (repeat n f)
  (map (lambda (i) (f)) (iota n)))

(define (flip . p) (cond [(null? p) (uniform-select '(#t #f))]
                         [else (rnd-select (list `(,(car p) #t) `(,(- (car p) 1.0) #f)))]))

(define (ex1)

  (define (flower shade)
    (node (data (color (gaussian shade 25)) (size .3))
          (petal shade)
          (petal shade)
          (petal shade)))
  (define (petal shade)
    (node (data (color (gaussian shade 25)) (size .3))))
  (repeat 10 (lambda () (flower 20))))

(bpm (ex1) 1 10)

(define (ex2)

  (define (flower shade)
    (node (data (color (gaussian 0 25)) (size .3))
          (petal shade)
          (petal shade)
          (petal shade)))

  (define (petal shade)
    (node (data (color (gaussian shade 25)) (size .3))))

  (repeat 10 (lambda () (flower (if (flip) 100 220)))))

(bpm (ex2) 1 10)

(define (ex3)
  (define (stem)
    (if (flip .2)
      (node (data (color 200) (size .5)))
      (node (data (color 200) (size .5)) (stem))))

  (repeat 5 stem))

(bpm (ex3) 1 10)

(define (ex4)
  (define (vine)
    (if (flip .1)
      (node (data (color 100) (size .1)))
      (node (data (color 100) (size .1)) (vine) (flower))))

  (define (flower)
    (node (data (color (gaussian 20 25)) (size .3))
          (petal 20)
          (petal 20)))

  (define (petal shade)
    (node (data (color (gaussian shade 25)) (size .3))))
  
  (vine))
(bpm (ex4) 1 10)

