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

(begin
  (for-each
    (lambda (i)
      (convert-sample->sxml (filename "s" i) (sample-grammar grammar) elements transforms))
    (iota arg)))

(define spacing (string->number (caddr (command-line))))

(define mode (string->number (cadddr (command-line))))

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

(begin
  (for-each
    (lambda (i)
      (convert-sample->sxml (filename "s" i) (sample-grammar grammar) elements transforms))
    (iota arg))
  )

