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

(define counter 0)
(pretty-print (string-append "bfs.dae.scene" (number->string counter)))
(define (filename prefix)
    (begin
        (set! counter (+ counter 1))
        (string-append prefix (number->string (- counter 1)))
))

(map (lambda (sample) (convert-sample->sxml (filename "bfs.dae.scene") sample elements transforms)) (map cadr (grammar-derivations grammar 0.4)))

;; have flag for generating one file with all the examples?