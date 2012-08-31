(import (rnrs)
        (bayes-program-merging)
        (printing)
        (program))

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

(define some-prog
  '(let () (define F81 (lambda () (F79 "elt1")))
     (define F80
       (lambda (V81 V82)
         (data (name "tr") (offset (vec V82 V81)))))
     (define F79
       (lambda (V80)
         (model
           (data (name V80) (pos (vec 0 0)) (local_scale 1.0)))))
     (define F78
       (lambda (V76 V77)
         ((lambda (V78)
            ((lambda (V79)
               (translate (F80 V78 V79)
                          (asm (data) (F81) (F77 V77 V76))))
             V76))
          V77)))
     (define F77
       (lambda (V74 V75) (translate (F80 V74 V75) (F79 "elt2"))))
     (lambda ()
       (choose
         (asm (data) (F81) (F77 0 12) (F77 0 -12) (F77 12 0)
              (F77 -12 0))
         (asm (data) (F81) (F78 12 0) (F78 -12 0) (F78 0 12)
              (F78 0 -12)))))
  )

(pretty-print (bpm test-data 100 1.0 5.0))


