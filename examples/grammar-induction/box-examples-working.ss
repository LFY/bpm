(define grammar-sequence
  (list
    '((program
        ((abstraction F16 () (elem "other" (tr "forward" (F0))))
         (abstraction F15 () (elem "other" (tr "forward" (F1))))
         (abstraction F0 () (elem "red"))
         (abstraction F1 () (elem "blue"))
         (abstraction F2 ()
                      (elem "root" (tr "red_chain" (F0))
                            (tr "blue_chain" (F1))))
         (abstraction F7 ()
                      (elem "root" (tr "red_chain" (F16))
                            (tr "blue_chain" (F15))))
         (abstraction F10 () (elem "other" (tr "forward" (F16))))
         (abstraction F13 () (elem "other" (tr "forward" (F15))))
         (abstraction F14 ()
                      (elem "root" (tr "red_chain" (F10))
                            (tr "blue_chain" (F13)))))
        (lambda () (choose (F2) (F7) (F14)))
        ((0.0) (0.0) (0.0) (0.0) (0) (0) (0) (0) (0)
               (-1.0986122886681098 -1.0986122886681098
                                    -1.0986122886681098)))
      -92.29122117425374)

    ;; some method to take the 'diff' of two grammars: first sort them, then inspect each NT and compute the diff (what NT removed, what NT added)
    ;; look at NT's of new grammar, we should have one nonterminal that is not included in the set of NT's in the previous grammar (before sorting)
    ;; that gives us merged components
    ;; here we merged two examples, this is the kind of merge that just results in lossless compression, i believe

    '((program ;; merged F7 and F14
        ((abstraction F17 ()
                      (choose
                        (elem "root" (tr "red_chain" (F10))
                              (tr "blue_chain" (F13)))
                        (elem "root" (tr "red_chain" (F16))
                              (tr "blue_chain" (F15)))))
         (abstraction F16 () (elem "other" (tr "forward" (F0))))
         (abstraction F15 () (elem "other" (tr "forward" (F1))))
         (abstraction F0 () (elem "red"))
         (abstraction F1 () (elem "blue"))
         (abstraction F2 ()
                      (elem "root" (tr "red_chain" (F0))
                            (tr "blue_chain" (F1))))
         (abstraction F10 () (elem "other" (tr "forward" (F16))))
         (abstraction F13 () (elem "other" (tr "forward" (F15)))))
        (lambda () (choose (F2) (F17) (F17)))
        ((-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
                                                   (0.0) (0.0) (0) (0) (0)
                                                   (-1.6094379124341003 -0.916290731874155
                                                                        -0.916290731874155)))
      -90.43740368443181)

    ;; another lossless compression merge
    
    '((program
        ((abstraction F18 ()
                      (choose
                        (elem "root" (tr "red_chain" (F0))
                              (tr "blue_chain" (F1)))
                        (elem "root" (tr "red_chain" (F10))
                              (tr "blue_chain" (F13)))
                        (elem "root" (tr "red_chain" (F16))
                              (tr "blue_chain" (F15)))))
         (abstraction F16 () (elem "other" (tr "forward" (F0))))
         (abstraction F15 () (elem "other" (tr "forward" (F1))))
         (abstraction F0 () (elem "red"))
         (abstraction F1 () (elem "blue"))
         (abstraction F10 () (elem "other" (tr "forward" (F16))))
         (abstraction F13 () (elem "other" (tr "forward" (F15)))))
        (lambda () (choose (F18) (F18) (F18)))
        ((-1.0986122886681098 -1.0986122886681098
                              -1.0986122886681098)
         (0.0) (0.0) (0.0) (0.0) (0) (0)
         (-1.0986122886681093 -1.0986122886681093
                              -1.0986122886681093)))
      -88.29122117425372)

    ;; here we merged F10 and F13, F10 ended in a red block and F13 ended in a
    ;; blue block.  This decreases likelihood of exemplars, but improves DL,
    ;; with the consequence of not preserving the context sensitivity between
    ;; the left turn and the color at the end (i.e., a relation between
    ;; geometry and object identity, which is not encoded a-priori, unlike
    ;; ad-hoc techniques of controlling the swapping of subtrees in a naive
    ;; algorithm)

    '((program
        ((abstraction F19 ()
                      (choose (elem "other" (tr "forward" (F16)))
                              (elem "other" (tr "forward" (F15)))))
         (abstraction F18 ()
                      (choose
                        (elem "root" (tr "red_chain" (F0))
                              (tr "blue_chain" (F1)))
                        (elem "root" (tr "red_chain" (F19))
                              (tr "blue_chain" (F19)))
                        (elem "root" (tr "red_chain" (F16))
                              (tr "blue_chain" (F15)))))
         (abstraction F16 () (elem "other" (tr "forward" (F0))))
         (abstraction F15 () (elem "other" (tr "forward" (F1))))
         (abstraction F0 () (elem "red"))
         (abstraction F1 () (elem "blue")))
        (lambda () (choose (F18) (F18) (F18)))
        ((-0.6931471805599453 -0.6931471805599453)
         (-1.0986122886681098 -1.0986122886681098
                              -1.0986122886681098)
         (0.0) (0.0) (0.0) (0.0)
         (-1.0986122886681093 -1.0986122886681093
                              -1.0986122886681093)))
      -87.67751553537363)

    ;; This just further merges F16 and F15. No difference is made in the set
    ;; of possible models but the likelihood of the exemplars has decreased
    
    '((program
        ((abstraction F20 ()
                      (choose (elem "other" (tr "forward" (F1)))
                              (elem "other" (tr "forward" (F0)))))
         (abstraction F19 ()
                      (choose (elem "other" (tr "forward" (F20)))
                              (elem "other" (tr "forward" (F20)))))
         (abstraction F18 ()
                      (choose
                        (elem "root" (tr "red_chain" (F0))
                              (tr "blue_chain" (F1)))
                        (elem "root" (tr "red_chain" (F19))
                              (tr "blue_chain" (F19)))
                        (elem "root" (tr "red_chain" (F20))
                              (tr "blue_chain" (F20)))))
         (abstraction F0 () (elem "red"))
         (abstraction F1 () (elem "blue")))
        (lambda () (choose (F18) (F18) (F18)))
        ((-0.6931471805599453 -0.6931471805599453)
         (-0.6931471805599453 -0.6931471805599453)
         (-1.0986122886681098 -1.0986122886681098
                              -1.0986122886681098)
         (0.0) (0.0)
         (-1.0986122886681093 -1.0986122886681093
                              -1.0986122886681093)))
      -87.06380989649351)

    ;; A merge that introduces a recursion. F21 considers chains of arbitrary
    ;; length that end in either a red or blue block.
    
    '((program
        ((abstraction F21 ()
                      (choose (elem "other" (tr "forward" (F21)))
                              (elem "other" (tr "forward" (F1)))
                              (elem "other" (tr "forward" (F0)))))
         (abstraction F18 ()
                      (choose
                        (elem "root" (tr "red_chain" (F0))
                              (tr "blue_chain" (F1)))
                        (elem "root" (tr "red_chain" (F21))
                              (tr "blue_chain" (F21)))
                        (elem "root" (tr "red_chain" (F21))
                              (tr "blue_chain" (F21)))))
         (abstraction F0 () (elem "red"))
         (abstraction F1 () (elem "blue")))
        (lambda () (choose (F18) (F18) (F18)))
        ((-1.6094379124341003 -0.916290731874155
                              -0.916290731874155)
         (-1.6094379124341003 -0.916290731874155
                              -0.916290731874155)
         (0.0) (0.0)
         (-1.0986122886681093 -1.0986122886681093
                              -1.0986122886681093)))
      -82.73093862127453)

    ;; Now we are looking at a different 'best grammar' in the beam search that
    ;; is not superfinite, but just more compactly encodes the grammar before
    ;; the last one.
    
    '((program
        ((abstraction F20 () (elem "other" (tr "forward" (F19))))
         (abstraction F19 ()
                      (choose (elem "other" (tr "forward" (F1)))
                              (elem "other" (tr "forward" (F0)))))
         (abstraction F18 ()
                      (choose
                        (elem "root" (tr "red_chain" (F0))
                              (tr "blue_chain" (F1)))
                        (elem "root" (tr "red_chain" (F20))
                              (tr "blue_chain" (F20)))
                        (elem "root" (tr "red_chain" (F19))
                              (tr "blue_chain" (F19)))))
         (abstraction F0 () (elem "red"))
         (abstraction F1 () (elem "blue")))
        (lambda () (choose (F18) (F18) (F18)))
        ((0) (-0.6931471805599453 -0.6931471805599453)
             (-1.0986122886681098 -1.0986122886681098
                                  -1.0986122886681098)
             (0.0) (0.0)
             (-1.0986122886681093 -1.0986122886681093
                                  -1.0986122886681093)))
      -81.8596004420913)

    ;; We end up with the superfinite version of that grammar.

    ((program
       ((abstraction F21 ()
                     (choose
                       (elem "root" (tr "red_chain" (F0))
                             (tr "blue_chain" (F1)))
                       (elem "root" (tr "red_chain" (F20))
                             (tr "blue_chain" (F20)))))
        (abstraction F20 ()
                     (choose (elem "other" (tr "forward" (F20)))
                             (elem "other" (tr "forward" (F1)))
                             (elem "other" (tr "forward" (F0)))))
        (abstraction F0 () (elem "red"))
        (abstraction F1 () (elem "blue")))
       (lambda () (choose (F21) (F21) (F21)))
       ((-1.0986122886681098 -0.4054651081081645)
        (-1.6094379124341003 -0.916290731874155
                             -0.916290731874155)
        (0.0) (0.0)
        (-1.0986122886681093 -1.0986122886681093
                             -1.0986122886681093)))
     -71.18992629708532)
    ))
