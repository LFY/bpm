(import (printing)
        (scene-graphs))

(define filename (cadr (command-line)))

(load filename)

(pretty-print `(define sample (quote (,(sample-grammar+parameters grammar)))))
(pretty-print `(define transforms (quote ,transforms)))
