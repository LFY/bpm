(define (find-best-reps-interleave-substitution eq-classes)
  (define (loop acc classes)
    (if (null? classes) acc
      (let* (
             [mcv (most-common-variable classes acc)]
             [new-class-reps (calculate-new-representatives mcv classes)]
             [work-list (substitute-with-representatives
                          new-class-reps 
                          (filter no-representative? new-class-reps))] 
             )
        (loop (append current-results acc) work-list))))
  (loop '() eq-classes))
