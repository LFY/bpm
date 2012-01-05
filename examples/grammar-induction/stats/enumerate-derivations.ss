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

(define spacing (string->number (caddr (command-line))))

(define mode (string->number (cadddr (command-line))))

(define arg (string->number (caddr (cddr (command-line)))))

(define derivations (grammar-derivations grammar mode arg))

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

;; creates both individual s-expression file as well as one s-expression file that contains all the derivations
(begin
    
    (map 
        (lambda (sample) 
            (let* ([prob (car sample)]
                [derivation (cadr sample)])
                (convert-sample->sxml (filename "d" (exp prob)) derivation elements transforms)))
        derivations)
    
    (convert-sample->sxml-multiple (filename "d" (exp (cum-distribution derivations))) (map cadr derivations) elements transforms spacing)
)