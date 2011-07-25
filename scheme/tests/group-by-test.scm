(import (rnrs)
        (_srfi :1)
        (util)
        (printing))

(print (group-by eq? even? '(1 2 3 4 5)))
(pretty-print (group-by equal? first '(((1 2) 1)
                         ((1 2) 2)
                         ((2 1) 1)
                         ((2 2) 2))))

(print (median-split '(1 2 3 4 5)))

(print (sort < '(1 5 2 4 3)))

(print (list-subtract '(1 2 3 4) '(1 2 3)))
