(import (rnrs)
        (bayes-program-merging)
        (printing))

(define test-data
  (list
    '(asm (data)
          (model
            (data (name "elt1") (pos (vec 0 0))
                  (local_scale 1.0)))
          (translate (data (name "tr") (offset (vec 12 0)))
                     (model
                       (data (name "elt2") (pos (vec 0 0))
                             (local_scale 1.0))))
          (translate (data (name "tr") (offset (vec -12 0)))
                     (model
                       (data (name "elt2") (pos (vec 0 0))
                             (local_scale 1.0))))
          (translate (data (name "tr") (offset (vec 0 12)))
                     (model
                       (data (name "elt2") (pos (vec 0 0))
                             (local_scale 1.0))))
          (translate (data (name "tr") (offset (vec 0 -12)))
                     (model
                       (data (name "elt2") (pos (vec 0 0))
                             (local_scale 1.0)))))
    '(asm (data)
          (model
            (data (name "elt1") (pos (vec 0 0))
                  (local_scale 1.0)))
          (translate (data (name "tr") (offset (vec 12 0)))
                     (asm (data)
                          (model
                            (data (name "elt1") (pos (vec 0 0))
                                  (local_scale 1.0)))
                          (translate (data (name "tr") (offset (vec 12 0)))
                                     (model
                                       (data (name "elt2") (pos (vec 0 0))
                                             (local_scale 1.0))))))
          (translate (data (name "tr") (offset (vec -12 0)))
                     (asm (data)
                          (model
                            (data (name "elt1") (pos (vec 0 0))
                                  (local_scale 1.0)))
                          (translate (data (name "tr") (offset (vec -12 0)))
                                     (model
                                       (data (name "elt2") (pos (vec 0 0))
                                             (local_scale 1.0))))))
          (translate (data (name "tr") (offset (vec 0 12)))
                     (asm (data)
                          (model
                            (data (name "elt1") (pos (vec 0 0))
                                  (local_scale 1.0)))
                          (translate (data (name "tr") (offset (vec 0 12)))
                                     (model
                                       (data (name "elt2") (pos (vec 0 0))
                                             (local_scale 1.0))))))
          (translate (data (name "tr") (offset (vec 0 -12)))
                     (asm (data)
                          (model
                            (data (name "elt1") (pos (vec 0 0))
                                  (local_scale 1.0)))
                          (translate (data (name "tr") (offset (vec 0 -12)))
                                     (model
                                       (data (name "elt2") (pos (vec 0 0))
                                             (local_scale 1.0)))))))

    ))

(pretty-print (bpm test-data 100 1.0 5.0))

