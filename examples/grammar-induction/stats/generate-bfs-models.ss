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

(define prob-threshold (string->number (caddr (command-line))))

(define mode (string->number (cadddr (command-line))))

(define counter 0)
(define (filename prefix)
    (begin
        (set! counter (+ counter 1))
        (string-append prefix (number->string (- counter 1)))
))

(define derivations (grammar-derivations grammar prob-threshold))

(pretty-print (length derivations))

(cond [(= mode 0) 
        (convert-sample->sxml-multiple (filename "bfs.dae.scene") (map cadr derivations) elements transforms 50.0)]
    [(= mode 1)
        (map (lambda (sample) (convert-sample->sxml (filename "bfs.dae.scene") sample elements transforms)) (map cadr derivations))])

