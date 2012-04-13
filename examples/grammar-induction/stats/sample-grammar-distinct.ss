(import 
        (_srfi :1)
        (_srfi :69)
        (printing)
        (chart-parsing)
        (grammar-likelihood)
        (parameter-estimation)
        (grammar-derivations-spread)
        (program)
        (node-constructors)
        (delimcc-simple-ikarus)
        (util)
        (scene-graphs)
)

(define all-samples (make-hash-table equal?))

(define transforms 'undefined)

(load (cadr (command-line)))

(define arg (string->number (caddr (cddr (command-line)))))

(define counter 0)

(define (filename prefix prob)
  (begin
    (set! counter (+ counter 1))
    (let* 
      ([count-string (number->string (- counter 1))]
       [prob-string (number->string prob)]
       )
      (string-append prefix (string-append "_" (string-append count-string (string-append "_" prob-string))))
      )))

(define (new-sample? sample)
  (if (hash-table-exists? all-samples sample) #f
    (begin
      (hash-table-set! all-samples sample 'exists) #t)))

(define (loop left)
  (if (= left 0) '()
    (let* ([sample (sample-grammar grammar)]
           [prob (single-data-grammar->likelihood (list sample) grammar)]
           [db (pretty-print left)])
      (if (new-sample? sample)
        (begin
          (with-output-to-file
            (filename "d" (exp prob))
            (lambda () (begin
                         (pretty-print `(define sample (quote (,sample))))
                         (pretty-print `(define transforms (quote ,transforms))))))
          (loop (- left 1)))
        (loop left)))))

(loop arg)
