(import (write-boxes)
        (printing)
        (scene-graphs)
        (_srfi :1))
        
(define sharing-program
  '(program
     ((abstraction F162 () (F161 (F160)))
      (abstraction F161 (V192)
                   (elem "root" (tr "right" V192) (tr "left" V192)))
      (abstraction F160 ()
                   ((lambda (V191) (elem "node" (tr "forward" V191)))
                    (choose (elem "node") (F160) (F160) (F160) (F160)))))
     (lambda ()
       (choose (F161 (elem "node")) (F162) (F162) (F162)
               (F162) (F162)))))

(define sharing-grammar
  '(program
     ((abstraction F162 () (F161 (F160)))
      (abstraction F161 (V192)
                   (elem "root" (tr "right" V192) (tr "left" V192)))
      (abstraction F160 ()
                   ((lambda (V191) (elem "node" (tr "forward" V191)))
                    (choose (elem "node") (F160) (F160) (F160) (F160)))))
     (lambda ()
       (choose (F161 (elem "node")) (F162) (F162) (F162)
               (F162) (F162)))))

(define long-range-grammar
  '(program
    ((abstraction F81 ()
                  (choose (elem "other" (tr "forward" (F24)))
                          (elem "other" (tr "forward" (F29)))
                          (elem "other" (tr "forward" (F81)))))
     (abstraction F72 ()
                  (choose
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F24))
                          (tr "blue_chain" (F29)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))))
     (abstraction F24 () (elem "red"))
     (abstraction F29 () (elem "blue")))
    (lambda () (choose (F72) (F72) (F72) (F72) (F72) (F72)))))

(define short-range-grammar
  '(program
     ((abstraction F82 ()
                   (choose (elem "blue" (tr "forward" (F82)))
                           (elem "blue")))
      (abstraction F77 ()
                   (choose (elem "red") (elem "red" (tr "forward" (F77)))))
      (abstraction F72 ()
                   (choose
                     (elem "root" (tr "red_chain" (F77))
                           (tr "blue_chain" (F82)))
                     (elem "root" (tr "red_chain" (F77))
                           (tr "blue_chain" (F82)))
                     (elem "root" (tr "red_chain" (F77))
                           (tr "blue_chain" (F82)))
                     (elem "root" (tr "red_chain" (F77))
                           (tr "blue_chain" (F82)))
                     (elem "root" (tr "red_chain" (F77))
                           (tr "blue_chain" (F82)))
                     (elem "root" (tr "red_chain" (F77))
                           (tr "blue_chain" (F82))))))
     (lambda () (choose (F72) (F72) (F72) (F72) (F72) (F72)))))

(define med-range-grammar
  '(program
  ((abstraction F105 ()
     (choose (elem "red" (tr "forward" (F105))) (elem "red")))
    (abstraction F100 ()
      (choose (elem "blue")
        (elem "blue" (tr "forward" (F100)))))
    (abstraction F99 ()
      (choose (elem "other" (tr "forward" (F100)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F100)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))))
    (abstraction F84 ()
      (choose
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99))))))
  (lambda () (choose (F84) (F84) (F84) (F84) (F84) (F84)))))

(define (short-range-examples num-examples)
  (define (chain color n)
    (cond [(= 0 n) `(elem ,color)]
          [else `(elem ,color (tr "forward" ,(chain color (- n 1))))]))

  (define (example n)
    `(elem "root"
           (tr "red_chain" ,(chain "red" n))
           (tr "blue_chain" ,(chain "blue" n))))

  (define data
    (map example (iota num-examples)))
  data)

(define (med-range-examples num-examples)
  (define (chain color n)
    (cond [(= 0 n) `(elem ,color)]
          [else `(elem ,color (tr "forward" ,(chain color (- n 1))))]))

  (define (chain-other n rest)
    (cond [(= 0 n) rest]
          [else `(elem "other"
                       (tr "forward"
                           ,(chain-other (- n 1) rest)))]))
  (define (example n)
    `(elem "root"
           (tr "red_chain" ,(chain-other 1 (chain "red" n)))
           (tr "blue_chain" ,(chain-other 1 (chain "blue" n)))))

  (define data
    (map example (iota num-examples)))
  data)

(define (long-range-examples num-examples)
  (define (chain color n)
    (cond [(= 0 n) `(elem ,color)]
          [else `(elem "other" (tr "forward" ,(chain color (- n 1))))]))

  (define (example n)
    `(elem "root"
           (tr "red_chain" ,(chain "red" n))
           (tr "blue_chain" ,(chain "blue" n))))

  (define data
    (map example (iota num-examples)))
  data)

(define (sharing-examples num-examples)
  (define (chain n)
    (cond [(= 0 n) '(elem "node")]
          [else `(elem "node" (tr "forward" ,(chain (- n 1))))]))

  (define (example n)
    `(elem "root"
           (tr "right" ,(chain n))
           (tr "left" ,(chain n))))
  (define data
    (map example (iota num-examples)))
  data)

(define grammar-examples
  (zip 
    (list "sharing" "short-range" "med-range" "long-range")
    (list sharing-grammar short-range-grammar med-range-grammar long-range-grammar)
    (list sharing-examples short-range-examples med-range-examples long-range-examples)))

(define (print-one num-examples g-ex)
  (define (write-graffle filename example)
    (begin
      (system (format "rm ~s" filename))
      (with-output-to-file filename
                           (lambda () (pretty-print (box-scene->graffle example))))
      (system (string-append "python serialize_graffle.py " filename))))

  (define (write-several-graffle basename tag sgs)
    (map (lambda (i) (write-graffle (string-append basename "_" tag "_" (number->string i) ".sxml") (list-ref sgs i))) (iota (length sgs))))

  (define (write-grammar basename tag grammar)
    (let* ([filename (string-append basename "_" tag ".ss")])
      (begin
        (system (format "rm ~s" filename))
        (with-output-to-file filename
                             (lambda () (pretty-print grammar)))
        )))

  (let* ([basename (car g-ex)]
         [grammar (cadr g-ex)]
         [examples ((caddr g-ex) num-examples)]
         [samples (map (lambda (i) (sample-grammar grammar)) (iota num-examples))]
         )
    (begin
      (write-several-graffle basename "training" examples)
      (write-several-graffle basename "samples" samples)
      (write-grammar basename "grammar" grammar))))

(map
  (lambda (g-ex)
    (print-one 6 g-ex))
  grammar-examples)
      



