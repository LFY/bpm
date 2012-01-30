(import (mcmc)
        (grammar-proposals)
        (grammar-likelihood)
        (printing)
        (_srfi :1))

(define test-data
  '((elem "elt0"
      (tr "trans107"
          (elem
            "elt17"
            (tr "trans110" (elem "elt7" (tr "trans111" (elem "elt21"))))
            (tr "trans109" (elem "elt20"))
            (tr "trans108" (elem "elt28"))))
      (tr "trans106" (elem "elt20"))
      (tr "trans102"
          (elem
            "elt10"
            (tr "trans105" (elem "elt27"))
            (tr "trans104" (elem "elt32"))
            (tr "trans103" (elem "elt33"))))
      (tr "trans101" (elem "elt28")))
     (elem "elt12"
       (tr "trans99"
           (elem
             "elt17"
             (tr "trans34" (elem "elt21"))
             (tr "trans100" (elem "elt28"))))
       (tr "trans98" (elem "elt9")) (tr "trans97" (elem "elt21"))
       (tr "trans96" (elem "elt15")) (tr "trans95" (elem "elt22"))
       (tr "trans93" (elem "elt10" (tr "trans94" (elem "elt21"))))
       (tr "trans92" (elem "elt32")))
     (elem "elt12"
       (tr "trans24"
           (elem
             "elt10"
             (tr "trans91" (elem "elt3"))
             (tr "trans25" (elem "elt2"))))
       (tr "trans90" (elem "elt8")) (tr "trans89" (elem "elt6"))
       (tr "trans88" (elem "elt6"))
       (tr "trans83"
           (elem
             "elt9"
             (tr "trans84"
                 (elem "elt17" (tr "trans87" (elem "elt21"))
                   (tr "trans29" (elem "elt5"))
                   (tr "trans86" (elem "elt18"))
                   (tr "trans31" (elem "elt19"))
                   (tr "trans85" (elem "elt25"))))))
       (tr "trans82" (elem "elt11")))
     (elem "elt12"
       (tr "trans76"
           (elem "elt17"
             (tr "trans80"
                 (elem
                   "elt7"
                   (tr "trans39" (elem "elt24"))
                   (tr "trans81" (elem "elt6"))))
             (tr "trans79" (elem "elt31")) (tr "trans78" (elem "elt29"))
             (tr "trans77" (elem "elt6"))))
       (tr "trans75" (elem "elt13")) (tr "trans74" (elem "elt6"))
       (tr "trans73" (elem "elt1")) (tr "trans72" (elem "elt2"))
       (tr "trans71" (elem "elt30"))
       (tr "trans61"
           (elem
             "elt14"
             (tr "trans63"
                 (elem "elt17" (tr "trans70" (elem "elt9"))
                   (tr "trans69" (elem "elt6"))
                   (tr "trans66"
                       (elem
                         "elt14"
                         (tr "trans68" (elem "elt2"))
                         (tr "trans67" (elem "elt26"))))
                   (tr "trans51"
                       (elem
                         "elt10"
                         (tr "trans53" (elem "elt27"))
                         (tr "trans65" (elem "elt29"))
                         (tr "trans64" (elem "elt13"))))))
             (tr "trans62" (elem "elt6")))))
     (elem "elt17"
       (tr "trans58"
           (elem
             "elt14"
             (tr "trans60" (elem "elt13"))
             (tr "trans59" (elem "elt21"))))
       (tr "trans55"
           (elem
             "elt14"
             (tr "trans57" (elem "elt20"))
             (tr "trans56" (elem "elt20"))))
       (tr "trans54" (elem "elt28"))
       (tr "trans51"
           (elem
             "elt10"
             (tr "trans53" (elem "elt27"))
             (tr "trans52" (elem "elt2"))))
       (tr "trans47"
           (elem
             "elt14"
             (tr "trans50" (elem "elt26"))
             (tr "trans49" (elem "elt25"))
             (tr "trans48" (elem "elt20")))))
     (elem "elt12"
       (tr "trans43"
           (elem
             "elt14"
             (tr "trans46" (elem "elt2"))
             (tr "trans45" (elem "elt8"))
             (tr "trans44" (elem "elt3"))))
       (tr "trans42" (elem "elt6")) (tr "trans41" (elem "elt6"))
       (tr "trans36"
           (elem "elt7" (tr "trans40" (elem "elt6"))
             (tr "trans39" (elem "elt24")) (tr "trans38" (elem "elt23"))
             (tr "trans37" (elem "elt22"))))
       (tr "trans35" (elem "elt1")))
     (elem "elt17" (tr "trans34" (elem "elt21"))
       (tr "trans33" (elem "elt20")) (tr "trans32" (elem "elt15"))
       (tr "trans31" (elem "elt19")) (tr "trans30" (elem "elt18"))
       (tr "trans29" (elem "elt5")) (tr "trans28" (elem "elt18"))
       (tr "trans27" (elem "elt11")))
     (elem "elt12"
       (tr "trans24"
           (elem
             "elt10"
             (tr "trans26" (elem "elt3"))
             (tr "trans25" (elem "elt2"))))
       (tr "trans23" (elem "elt6")) (tr "trans22" (elem "elt8"))
       (tr "trans21" (elem "elt16")) (tr "trans20" (elem "elt15"))
       (tr "trans13"
           (elem "elt12"
             (tr "trans17"
                 (elem
                   "elt14"
                   (tr "trans19" (elem "elt9"))
                   (tr "trans18" (elem "elt8"))))
             (tr "trans16" (elem "elt13")) (tr "trans15" (elem "elt6"))
             (tr "trans14" (elem "elt1")))))
     (elem "elt0"
       (tr "trans8"
           (elem
             "elt9"
             (tr "trans9"
                 (elem
                   "elt10"
                   (tr "trans12" (elem "elt4"))
                   (tr "trans11" (elem "elt2"))
                   (tr "trans10" (elem "elt11"))))))
       (tr "trans6" (elem "elt7" (tr "trans7" (elem "elt8"))))
       (tr "trans5" (elem "elt6")) (tr "trans4" (elem "elt5"))
       (tr "trans3" (elem "elt4")) (tr "trans2" (elem "elt3"))
       (tr "trans1" (elem "elt2")) (tr "trans0" (elem "elt1")))))

(define (print-score state)
  (let* ([curr-state (list-ref state 1)]
         [next-state (list-ref state 2)]
         [correction (list-ref state 4)]
         [score-ratio (list-ref state 3)]
         [accept? (list-ref state 5)])
    (begin
      (if accept? (pretty-print (list next-state (grammar->posterior test-data next-state)))))))

(run-mcmc
  (init-grammar test-data)
  (split-merge-proposal test-data)
  (lambda (next curr) (- (grammar->posterior test-data next)
                         (grammar->posterior test-data curr)))
  (num-iter-stop print-score 100))

