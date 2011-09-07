
(import (beam-learning)
        (_srfi :1)
        (printing))

(define data (list 
              '(node (data (name NULL))
                     (node
                       (data (name HTML) (left 0) (top 0) (width 962)
                             (height 812))
                       (node
                         (data (name BODY) (left 0) (top 0) (width 962)
                               (height 716))
                         (node
                           (data (name DIV) (left 0) (top 0) (width 910)
                                 (height 716))
                           (node
                             (data (name DIV) (left 50) (top 67) (width 250)
                                   (height 377))
                             (node
                               (data (name A) (left 50) (top 67) (width 244)
                                     (height 118)))
                             (node
                               (data (name UL) (left 50) (top 238) (width 250)
                                     (height 115))
                               (node
                                 (data (name LI) (left 50) (top 238)
                                       (width 250) (height 23)))
                               (node
                                 (data (name LI) (left 50) (top 261)
                                       (width 250) (height 23)))
                               (node
                                 (data (name LI) (left 50) (top 284)
                                       (width 250) (height 23)))
                               (node
                                 (data (name LI) (left 50) (top 307)
                                       (width 250) (height 23)))
                               (node
                                 (data (name LI) (left 50) (top 330)
                                       (width 250) (height 23)))))
                           (node
                             (data (name DIV) (left 325) (top 238) (width 575)
                                   (height 345))
                             (node
                               (data (name H1) (left 325) (top 238) (width 575)
                                     (height 23)))
                             (node
                               (data (name P) (left 325) (top 266) (width 575)
                                     (height 72)))
                             (node
                               (data (name H2) (left 325) (top 356) (width 575)
                                     (height 23)))
                             (node
                               (data (name P) (left 325) (top 384) (width 575)
                                     (height 54)))
                             (node
                               (data (name P) (left 325) (top 456) (width 575)
                                     (height 54))))
                           (node
                             (data (name DIV) (left 50) (top 583) (width 850)
                                   (height 122))
                             (node
                               (data (name P) (left 325) (top 594) (width 575)
                                     (height 48))))))))))

(pretty-print (learn-model data 1 10))
