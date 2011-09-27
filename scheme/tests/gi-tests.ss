(import (printing)
        (util)
        (grammar-induction)
        (scene-graphs)
        (profiling))


(define test-data '((elem
      "elt0"
      (tr "trans85"
          (elem "elt5"
            (tr "trans101"
                (elem
                  "elt0"
                  (tr "trans102"
                      (elem
                        "elt6"
                        (tr "trans103" (elem "elt15"))
                        (tr "trans12" (elem "elt2"))
                        (tr "trans11" (elem "elt2"))))))
            (tr "trans100" (elem "elt2")) (tr "trans7" (elem "elt2"))
            (tr "trans14" (elem "elt2"))
            (tr "trans86"
                (elem
                  "elt4"
                  (tr "trans87"
                      (elem
                        "elt1"
                        (tr "trans96"
                            (elem
                              "elt4"
                              (tr "trans97"
                                  (elem
                                    "elt1"
                                    (tr "trans99" (elem "elt14"))
                                    (tr "trans35" (elem "elt2"))
                                    (tr "trans98" (elem "elt10"))))))
                        (tr "trans95" (elem "elt3"))
                        (tr "trans48"
                            (elem
                              "elt4"
                              (tr "trans49"
                                  (elem
                                    "elt1"
                                    (tr "trans93"
                                        (elem
                                          "elt7"
                                          (tr "trans94"
                                              (elem
                                                "elt8"
                                                (tr "trans17"
                                                    (elem "elt9"))))))
                                    (tr "trans50" (elem "elt10"))
                                    (tr "trans50"
                                        (elem
                                          "elt4"
                                          (tr "trans20"
                                              (elem "elt1"
                                                (tr "trans89"
                                                    (elem
                                                      "elt4"
                                                      (tr "trans87"
                                                          (elem
                                                            "elt1"
                                                            (tr "trans92"
                                                                (elem
                                                                  "elt3"))
                                                            (tr "trans91"
                                                                (elem
                                                                  "elt10"))
                                                            (tr "trans90"
                                                                (elem
                                                                  "elt2"))))))
                                                (tr "trans35"
                                                    (elem "elt2"))
                                                (tr "trans20"
                                                    (elem
                                                      "elt16"
                                                      (tr "trans20"
                                                          (elem
                                                            "elt17"
                                                            (tr "trans20"
                                                                (elem
                                                                  "elt18"))))))
                                                (tr "trans88"
                                                    (elem
                                                      "elt10")))))))))))))))))))

(define result-grammar (gi-bmm test-data 1))

(print "profiling stats:")
(pretty-print (current-profiling-stats))

;; sample of what a resulting grammar is supposed ot look like
'(define result-grammar '(program
  ((abstraction F55 ()
     (choose
       (elem "elt1" (tr "trans96" (F54))
         (tr "trans95" (F46)) (tr "trans48" (F54)))
       (elem "elt1" (tr "trans93" (F16))
         (tr "trans50" (F45)) (tr "trans50" (F54)))
       (elem "elt1" (tr "trans99" (F8)) (tr "trans35" (F42))
         (tr "trans98" (F45)))
       (elem "elt1" (tr "trans89" (F54))
         (tr "trans35" (F42)) (tr "trans20" (F26))
         (tr "trans88" (F45)))
       (elem "elt1" (tr "trans92" (F46))
         (tr "trans91" (F45)) (tr "trans90" (F42)))))
    (abstraction F54 ()
      (choose (elem "elt4" (tr "trans87" (F55)))
        (elem "elt4" (tr "trans49" (F55)))
        (elem "elt4" (tr "trans97" (F55)))
        (elem "elt4" (tr "trans20" (F55)))
        (elem "elt4" (tr "trans87" (F55)))))
    (abstraction F53 ()
      (choose (elem "elt0" (tr "trans85" (F34)))
        (elem "elt0" (tr "trans102" (F3)))))
    (abstraction F46 () (choose (elem "elt3")))
    (abstraction F45 () (choose (elem "elt10")))
    (abstraction F42 () (choose (elem "elt2")))
    (abstraction F24 () (elem "elt18"))
    (abstraction F25 () (elem "elt17" (tr "trans20" (F24))))
    (abstraction F26 () (elem "elt16" (tr "trans20" (F25))))
    (abstraction F0 () (elem "elt15"))
    (abstraction F3 ()
      (elem "elt6" (tr "trans103" (F0)) (tr "trans12" (F42))
        (tr "trans11" (F42))))
    (abstraction F8 () (elem "elt14"))
    (abstraction F14 () (elem "elt9"))
    (abstraction F15 () (elem "elt8" (tr "trans17" (F14))))
    (abstraction F16 () (elem "elt7" (tr "trans94" (F15))))
    (abstraction F34 ()
      (elem "elt5" (tr "trans101" (F53))
        (tr "trans100" (F42)) (tr "trans7" (F42))
        (tr "trans14" (F42)) (tr "trans86" (F54)))))
  (lambda () (choose (F53)))))

(define sample (sample-grammar result-grammar))

(load "gi-test-playground-nodes.ss")

(print "Sample from the grammar")
(pretty-print sample)

(print "Reconstructed Collada nodes")
(pretty-print (reconstruct-dae sample elements transforms))

(print "To SXML:")
(sample->sxml "gi-test.sxml" result-grammar elements transforms)

(print "To scene sampler:")
(system "rm gi-sampler.ss")
(output-scene-sampler "gi-sampler.ss" result-grammar elements transforms "gi-test")
