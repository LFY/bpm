(import (write-boxes)
        (printing)
        (scene-graphs)
        (_srfi :1))
     
;; =============================================================================
;; Sharing (equal length branches)
;; =============================================================================

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
   ((abstraction F82 ()
      (choose (elem "node" (tr "forward" (F82)))
        (elem "node")))
     (abstraction F77 ()
       (choose
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82))))))
   (lambda () (choose (F77) (F77) (F77) (F77) (F77) (F77)))))


;; =============================================================================
;; Short-range context sensitivity (blue chains/red chains always on consistent
;; side of root)
;; =============================================================================
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

;; =============================================================================
;; Med-range context sensitivity (blue chains/red chains always on consistent
;; side of root, but there is an additional common element in-between)
;; =============================================================================

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

(define med-range-grammar
  '(program
   ((abstraction F102 ()
      (choose (elem "red" (tr "forward" (F102)))
        (elem "red")))
     (abstraction F99 ()
       (choose (elem "other" (tr "forward" (F102)))
         (elem "other" (tr "forward" (F102)))
         (elem "other" (tr "forward" (F50)))
         (elem "other" (tr "forward" (F102)))
         (elem "other" (tr "forward" (F78)))
         (elem "other" (tr "forward" (F102)))))
     (abstraction F94 () (elem "other" (tr "forward" (F93))))
     (abstraction F93 ()
       (choose (elem "blue")
         (elem "blue" (tr "forward" (F93)))))
     (abstraction F84 ()
       (choose
         (elem "root" (tr "red_chain" (F99)) ;; F99: red chains
           (tr "blue_chain" (F94))) ;; F94: blue chains
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))))
     (abstraction F78 () (elem "red" (tr "forward" (F102))))
     (abstraction F50 () (elem "red" (tr "forward" (F78)))))
   (lambda () (choose (F84) (F84) (F84) (F84) (F84) (F84)))))

;; =============================================================================
;; Long-range context sensitivity (blue ends/red ends always on consistent side
;; of root)
;; =============================================================================

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


;; (define grammar-examples
  ;; (zip 
    ;; (list "sharing-program" "sharing" "short-range" "med-range" "long-range")
    ;; (list sharing-program sharing-grammar short-range-grammar med-range-grammar long-range-grammar)
    ;; (list sharing-examples sharing-examples short-range-examples med-range-examples long-range-examples)))
(define grammar-examples
  (zip 
    (list "sharing")
    (list sharing-grammar)
    (list sharing-examples)))

(define (print-one num-examples g-ex)
  (define (write-graffle filename example)
    (begin
      (system (format "rm ~s" filename))
      (with-output-to-file filename
                           (lambda () (pretty-print (box-scene->graffle example))))
      (system (string-append "python serialize_graffle.py " filename))
      (system (format "rm ~s" filename))
      ))

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
    (print-one 12 g-ex))
  grammar-examples)
      



