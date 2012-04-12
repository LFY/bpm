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

(load (cadr (command-line)))

(define all-samples (make-hash-table equal?))

(define (loop sample-count total-prob)
  (let* ([smp (sample-grammar grammar)]
         [prob (single-data-grammar->likelihood (list smp) grammar)]
         [new-smp? (not (hash-table-exists? all-samples smp))]
         [new-total-prob (if new-smp? (+ total-prob (exp prob)) total-prob)]
         [new-sample-count (if new-smp? 
                             (begin
                               (hash-table-set! all-samples smp 'exists)
                               (+ sample-count 1))
                             sample-count)])
    (pretty-print (list new-sample-count new-total-prob))
    (cond [(> new-total-prob 0.90) (list new-sample-count new-total-prob)]
          [else (loop new-sample-count new-total-prob)])))

(define answer (loop 0 0.0))

(pretty-print
  `(,(car answer) models occupy ,(cadr answer) probability))
