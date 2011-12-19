Productions with probability > 0.01, for each grammar in the sequence:
# Grammar + top derivations:
(program
  ((abstraction F20 () (elem "gray" (tr "forward" (F19))))
    (abstraction F21 () (elem "blue"))
    (abstraction F22 () (elem "blue" (tr "forward" (F21))))
    (abstraction F23 () (elem "blue" (tr "forward" (F22))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F26 ()
      (elem "root" (tr "left" (F20)) (tr "right" (F25))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F6 ()
      (elem "root" (tr "left" (F2)) (tr "right" (F5))))
    (abstraction F8 () (elem "red" (tr "forward" (F0))))
    (abstraction F9 () (elem "gray" (tr "forward" (F8))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F12 () (elem "blue" (tr "forward" (F21))))
    (abstraction F13 () (elem "gray" (tr "forward" (F12))))
    (abstraction F14 () (elem "gray" (tr "forward" (F13))))
    (abstraction F15 ()
      (elem "root" (tr "left" (F10)) (tr "right" (F14))))
    (abstraction F17 () (elem "red" (tr "forward" (F0))))
    (abstraction F18 () (elem "red" (tr "forward" (F17))))
    (abstraction F19 () (elem "gray" (tr "forward" (F18)))))
  (lambda () (choose (F6) (F15) (F26)))
  ((0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098))
  (stats (posterior -148.60268968544437)
    (likelihood+weight -3.295836866004329 1.0)
    (prior+weight -145.30685281944005 1.0) (desc-length 146)
    (dirichlet-prior 0.6931471805599456))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue")))))))))))
# Grammar + top derivations:
(program
  ((abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F20 () (elem "gray" (tr "forward" (F19))))
    (abstraction F21 () (elem "blue"))
    (abstraction F22 () (elem "blue" (tr "forward" (F21))))
    (abstraction F23 () (elem "blue" (tr "forward" (F22))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F26 ()
      (elem "root" (tr "left" (F20)) (tr "right" (F25))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F6 ()
      (elem "root" (tr "left" (F2)) (tr "right" (F5))))
    (abstraction F9 () (elem "gray" (tr "forward" (F27))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F12 () (elem "blue" (tr "forward" (F21))))
    (abstraction F13 () (elem "gray" (tr "forward" (F12))))
    (abstraction F14 () (elem "gray" (tr "forward" (F13))))
    (abstraction F15 ()
      (elem "root" (tr "left" (F10)) (tr "right" (F14))))
    (abstraction F18 () (elem "red" (tr "forward" (F27))))
    (abstraction F19 () (elem "gray" (tr "forward" (F18)))))
  (lambda () (choose (F6) (F15) (F26)))
  ((0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098))
  (stats (posterior -142.60268968544437)
    (likelihood+weight -3.295836866004329 1.0)
    (prior+weight -139.30685281944005 1.0) (desc-length 140)
    (dirichlet-prior 0.6931471805599456))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue")))))))))))
# Grammar + top derivations:
(program
  ((abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F20 () (elem "gray" (tr "forward" (F19))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F26 ()
      (elem "root" (tr "left" (F20)) (tr "right" (F25))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F6 ()
      (elem "root" (tr "left" (F2)) (tr "right" (F5))))
    (abstraction F9 () (elem "gray" (tr "forward" (F27))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F13 () (elem "gray" (tr "forward" (F28))))
    (abstraction F14 () (elem "gray" (tr "forward" (F13))))
    (abstraction F15 ()
      (elem "root" (tr "left" (F10)) (tr "right" (F14))))
    (abstraction F18 () (elem "red" (tr "forward" (F27))))
    (abstraction F19 () (elem "gray" (tr "forward" (F18)))))
  (lambda () (choose (F6) (F15) (F26)))
  ((0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098))
  (stats (posterior -136.60268968544437)
    (likelihood+weight -3.295836866004329 1.0)
    (prior+weight -133.30685281944005 1.0) (desc-length 134)
    (dirichlet-prior 0.6931471805599456))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue")))))))))))
# Grammar + top derivations:
(program
  ((abstraction F29 ()
     (choose
       (elem "root" (tr "left" (F10)) (tr "right" (F14)))
       (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F20 () (elem "gray" (tr "forward" (F19))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F26 ()
      (elem "root" (tr "left" (F20)) (tr "right" (F25))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F9 () (elem "gray" (tr "forward" (F27))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F13 () (elem "gray" (tr "forward" (F28))))
    (abstraction F14 () (elem "gray" (tr "forward" (F13))))
    (abstraction F18 () (elem "red" (tr "forward" (F27))))
    (abstraction F19 () (elem "gray" (tr "forward" (F18)))))
  (lambda () (choose (F29) (F26)))
  ((-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (-0.6931471805599453 -0.6931471805599453))
  (stats (posterior -135.29583686600432)
    (likelihood+weight -3.295836866004329 1.0)
    (prior+weight -132.0 1.0) (desc-length 132)
    (dirichlet-prior 8.881784197001252e-16))
  ())
((0.25
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.25
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue"))))))))))
  (0.25
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F30 ()
     (choose
       (elem "root" (tr "left" (F20)) (tr "right" (F25)))
       (elem "root" (tr "left" (F10)) (tr "right" (F14)))
       (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F20 () (elem "gray" (tr "forward" (F19))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F9 () (elem "gray" (tr "forward" (F27))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F13 () (elem "gray" (tr "forward" (F28))))
    (abstraction F14 () (elem "gray" (tr "forward" (F13))))
    (abstraction F18 () (elem "red" (tr "forward" (F27))))
    (abstraction F19 () (elem "gray" (tr "forward" (F18)))))
  (lambda () (choose (F30)))
  ((-1.0986122886681098 -1.0986122886681098
     -1.0986122886681098)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0))
  (stats (posterior -132.60268968544437)
    (likelihood+weight -3.295836866004329 1.0)
    (prior+weight -129.30685281944005 1.0) (desc-length 130)
    (dirichlet-prior 0.6931471805599456))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue"))))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F31 ()
     (choose (elem "gray" (tr "forward" (F18)))
       (elem "gray" (tr "forward" (F13)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F20)) (tr "right" (F25)))
        (elem "root" (tr "left" (F10)) (tr "right" (F31)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F20 () (elem "gray" (tr "forward" (F31))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F9 () (elem "gray" (tr "forward" (F27))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F13 () (elem "gray" (tr "forward" (F28))))
    (abstraction F18 () (elem "red" (tr "forward" (F27)))))
  (lambda () (choose (F30)))
  ((-0.6931471805599453 -0.6931471805599453)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -132.98898404656427)
    (likelihood+weight -4.68213122712422 1.0)
    (prior+weight -128.30685281944005 1.0) (desc-length 129)
    (dirichlet-prior 0.6931471805599461))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.16666666666666666
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.16666666666666666
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue")))))))))))
# Grammar + top derivations:
(program
  ((abstraction F32 ()
     (choose (elem "gray" (tr "forward" (F28)))
       (elem "gray" (tr "forward" (F18)))
       (elem "gray" (tr "forward" (F32)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F20)) (tr "right" (F25)))
        (elem "root" (tr "left" (F10)) (tr "right" (F32)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F20 () (elem "gray" (tr "forward" (F32))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F9 () (elem "gray" (tr "forward" (F27))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F18 () (elem "red" (tr "forward" (F27)))))
  (lambda () (choose (F30)))
  ((-1.0986122886681098 -1.0986122886681098
     -1.0986122886681098)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -133.20537937088875)
    (likelihood+weight -6.591673732008658 1.0)
    (prior+weight -126.6137056388801 1.0) (desc-length 128)
    (dirichlet-prior 1.3862943611198912))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue"
                  (tr "forward"
                    (elem "blue"
                      (tr "forward" (elem "blue"))))))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red"
                  (tr "forward"
                    (elem "red" (tr "forward" (elem "red"))))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue"
                  (tr "forward"
                    (elem "blue"
                      (tr "forward" (elem "blue"))))))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red")))))))))))
# Grammar + top derivations:
(program
  ((abstraction F33 ()
     (choose (elem "gray" (tr "forward" (F33)))
       (elem "gray" (tr "forward" (F28)))
       (elem "gray" (tr "forward" (F18)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F33)) (tr "right" (F25)))
        (elem "root" (tr "left" (F10)) (tr "right" (F33)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F9 () (elem "gray" (tr "forward" (F27))))
    (abstraction F10 () (elem "gray" (tr "forward" (F9))))
    (abstraction F18 () (elem "red" (tr "forward" (F27)))))
  (lambda () (choose (F30)))
  ((-1.0986122886681098 -1.0986122886681098
     -1.0986122886681098)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -128.0684255882441)
    (likelihood+weight -7.454719949364001 1.0)
    (prior+weight -120.6137056388801 1.0) (desc-length 122)
    (dirichlet-prior 1.3862943611198912))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue"
                  (tr "forward"
                    (elem "blue"
                      (tr "forward" (elem "blue"))))))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue"
                  (tr "forward"
                    (elem "blue"
                      (tr "forward" (elem "blue"))))))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.037037037037037035
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue")))))))))))
# Grammar + top derivations:
(program
  ((abstraction F34 ()
     (choose (elem "gray" (tr "forward" (F34)))
       (elem "gray" (tr "forward" (F27)))))
    (abstraction F33 ()
      (choose (elem "gray" (tr "forward" (F33)))
        (elem "gray" (tr "forward" (F28)))
        (elem "gray" (tr "forward" (F18)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F33)) (tr "right" (F25)))
        (elem "root" (tr "left" (F34)) (tr "right" (F33)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F18 () (elem "red" (tr "forward" (F27)))))
  (lambda () (choose (F30)))
  ((-0.6931471805599453 -0.6931471805599453)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -128.454719949364)
    (likelihood+weight -8.841014310483892 1.0)
    (prior+weight -119.6137056388801 1.0) (desc-length 121)
    (dirichlet-prior 1.3862943611198917))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue"
                  (tr "forward"
                    (elem "blue"
                      (tr "forward" (elem "blue"))))))))))))
  (0.1111111111111111
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue"
                  (tr "forward"
                    (elem "blue"
                      (tr "forward" (elem "blue"))))))))))))
  (0.05555555555555555
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.05555555555555555
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.05555555555555555
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.05555555555555555
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.05555555555555555
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red")))))))))))
# Grammar + top derivations:
(program
  ((abstraction F35 ()
     (choose (elem "gray" (tr "forward" (F35)))
       (elem "gray" (tr "forward" (F28)))
       (elem "gray" (tr "forward" (F18)))
       (elem "gray" (tr "forward" (F27)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F35)) (tr "right" (F25)))
        (elem "root" (tr "left" (F35)) (tr "right" (F35)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F25 () (elem "gray" (tr "forward" (F24))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F18 () (elem "red" (tr "forward" (F27)))))
  (lambda () (choose (F30)))
  ((-1.3862943611198906 -1.3862943611198906
     -1.3862943611198906 -1.3862943611198906)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -123.26565016558033)
    (likelihood+weight -10.75055681536833 1.0)
    (prior+weight -112.515093350212 1.0) (desc-length 115)
    (dirichlet-prior 2.4849066497880026))
  ())
((0.3333333333333333
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.3333333333333333
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F36 ()
     (choose (elem "gray" (tr "forward" (F24)))
       (elem "gray" (tr "forward" (F36)))
       (elem "gray" (tr "forward" (F28)))
       (elem "gray" (tr "forward" (F18)))
       (elem "gray" (tr "forward" (F27)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F36)) (tr "right" (F36)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F24 () (elem "gray" (tr "forward" (F23))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F18 () (elem "red" (tr "forward" (F27)))))
  (lambda () (choose (F30)))
  ((-1.6094379124341003 -1.6094379124341003
     -1.6094379124341003 -1.6094379124341003
     -1.6094379124341003)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0)
    (0.0))
  (stats (posterior -115.05702285191936)
    (likelihood+weight -12.235076682267303 1.0)
    (prior+weight -102.82194616965205 1.0) (desc-length 106)
    (dirichlet-prior 3.178053830347948))
  ())
((0.5
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.5
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F37 ()
     (choose (elem "gray" (tr "forward" (F23)))
       (elem "gray" (tr "forward" (F37)))
       (elem "gray" (tr "forward" (F28)))
       (elem "gray" (tr "forward" (F18)))
       (elem "gray" (tr "forward" (F27)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F37)) (tr "right" (F37)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F27 () (elem "red" (tr "forward" (F0))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4))))
    (abstraction F18 () (elem "red" (tr "forward" (F27)))))
  (lambda () (choose (F30)))
  ((-1.6094379124341003 -1.6094379124341003
     -1.6094379124341003 -1.6094379124341003
     -1.6094379124341003)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -109.8218435634956)
    (likelihood+weight -12.999897393843561 1.0)
    (prior+weight -96.82194616965205 1.0) (desc-length 100)
    (dirichlet-prior 3.178053830347948))
  ())
((0.5
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020000000000000004
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red")))))))))
# Grammar + top derivations:
(program
  ((abstraction F38 ()
     (choose (elem "red" (tr "forward" (F38)))
       (elem "red" (tr "forward" (F0)))))
    (abstraction F37 ()
      (choose (elem "gray" (tr "forward" (F23)))
        (elem "gray" (tr "forward" (F37)))
        (elem "gray" (tr "forward" (F28)))
        (elem "gray" (tr "forward" (F38)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F37)) (tr "right" (F37)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F28 () (elem "blue" (tr "forward" (F21))))
    (abstraction F21 () (elem "blue"))
    (abstraction F23 () (elem "blue" (tr "forward" (F28))))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4)))))
  (lambda () (choose (F30)))
  ((-0.6931471805599453 -0.6931471805599453)
    (-1.3862943611198906 -1.3862943611198906
      -1.3862943611198906 -1.3862943611198906)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -105.73138606838006)
    (likelihood+weight -13.52314553760811 1.0)
    (prior+weight -92.20824053077195 1.0) (desc-length 94)
    (dirichlet-prior 1.7917594692280578))
  ())
((0.5
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.03125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.015625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.015625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F39 ()
     (choose (elem "blue" (tr "forward" (F39)))
       (elem "blue" (tr "forward" (F21)))))
    (abstraction F38 ()
      (choose (elem "red" (tr "forward" (F38)))
        (elem "red" (tr "forward" (F0)))))
    (abstraction F37 ()
      (choose (elem "gray" (tr "forward" (F39)))
        (elem "gray" (tr "forward" (F37)))
        (elem "gray" (tr "forward" (F38)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F37)) (tr "right" (F37)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F21 () (elem "blue"))
    (abstraction F0 () (elem "red"))
    (abstraction F1 () (elem "gray" (tr "forward" (F0))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4)))))
  (lambda () (choose (F30)))
  ((-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -101.35324650081272)
    (likelihood+weight -14.04639368137266 1.0)
    (prior+weight -87.30685281944005 1.0) (desc-length 88)
    (dirichlet-prior 0.693147180559947))
  ())
()
# Grammar + top derivations:
(program
  ((abstraction F40 ()
     (choose (elem "red") (elem "red" (tr "forward" (F40)))))
    (abstraction F39 ()
      (choose (elem "blue" (tr "forward" (F39)))
        (elem "blue" (tr "forward" (F21)))))
    (abstraction F37 ()
      (choose (elem "gray" (tr "forward" (F39)))
        (elem "gray" (tr "forward" (F37)))
        (elem "gray" (tr "forward" (F40)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F37)) (tr "right" (F37)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F21 () (elem "blue"))
    (abstraction F1 () (elem "gray" (tr "forward" (F40))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F21))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4)))))
  (lambda () (choose (F30)))
  ((-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0) (0.0) (0.0))
  (stats (posterior -97.68423106832846)
    (likelihood+weight -16.377378248888405 1.0)
    (prior+weight -81.30685281944005 1.0) (desc-length 82)
    (dirichlet-prior 0.693147180559947))
  ())
((0.25
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.25
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.25
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.25
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.25
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.0625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red"
                  (tr "forward"
                    (elem "red" (tr "forward" (elem "red"))))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F41 ()
     (choose (elem "blue")
       (elem "blue" (tr "forward" (F41)))))
    (abstraction F40 ()
      (choose (elem "red") (elem "red" (tr "forward" (F40)))))
    (abstraction F37 ()
      (choose (elem "gray" (tr "forward" (F41)))
        (elem "gray" (tr "forward" (F37)))
        (elem "gray" (tr "forward" (F40)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F37)) (tr "right" (F37)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F1 () (elem "gray" (tr "forward" (F40))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F4 () (elem "gray" (tr "forward" (F41))))
    (abstraction F5 () (elem "gray" (tr "forward" (F4)))))
  (lambda () (choose (F30)))
  ((-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453)
    (-1.0986122886681098 -1.0986122886681098
      -1.0986122886681098)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0) (0.0))
  (stats (posterior -85.66725025006379)
    (likelihood+weight -10.360397430623735 1.0)
    (prior+weight -75.30685281944005 1.0) (desc-length 76)
    (dirichlet-prior 0.693147180559947))
  ())
((0.125
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.125
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.0625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue"))))))))))
  (0.0625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.0625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue"))))))))))
  (0.0625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.0625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue"))))))))))
  (0.0625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F42 ()
     (choose (elem "gray" (tr "forward" (F41)))
       (elem "gray" (tr "forward" (F42)))
       (elem "gray" (tr "forward" (F40)))))
    (abstraction F41 ()
      (choose (elem "blue")
        (elem "blue" (tr "forward" (F41)))))
    (abstraction F40 ()
      (choose (elem "red") (elem "red" (tr "forward" (F40)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F42)) (tr "right" (F42)))
        (elem "root" (tr "left" (F2)) (tr "right" (F5)))))
    (abstraction F1 () (elem "gray" (tr "forward" (F40))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1))))
    (abstraction F5 () (elem "gray" (tr "forward" (F42)))))
  (lambda () (choose (F30)))
  ((-1.0986122886681098 -1.0986122886681098
     -1.0986122886681098)
    (-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0) (0.0))
  (stats (posterior -82.89079446133093)
    (likelihood+weight -13.58394164189088 1.0)
    (prior+weight -69.30685281944005 1.0) (desc-length 70)
    (dirichlet-prior 0.693147180559947))
  ())
((0.041666666666666664
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "blue" (tr "forward" (elem "blue"))))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "blue"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red")))))))))
# Grammar + top derivations:
(program
  ((abstraction F43 ()
     (choose (elem "gray" (tr "forward" (F43)))
       (elem "gray" (tr "forward" (F41)))
       (elem "gray" (tr "forward" (F40)))))
    (abstraction F41 ()
      (choose (elem "blue")
        (elem "blue" (tr "forward" (F41)))))
    (abstraction F40 ()
      (choose (elem "red") (elem "red" (tr "forward" (F40)))))
    (abstraction F30 ()
      (choose
        (elem "root" (tr "left" (F43)) (tr "right" (F43)))
        (elem "root" (tr "left" (F2)) (tr "right" (F43)))))
    (abstraction F1 () (elem "gray" (tr "forward" (F40))))
    (abstraction F2 () (elem "gray" (tr "forward" (F1)))))
  (lambda () (choose (F30)))
  ((-1.0986122886681098 -1.0986122886681098
     -1.0986122886681098)
    (-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0))
  (stats (posterior -79.7248650805185)
    (likelihood+weight -16.418012261078438 1.0)
    (prior+weight -63.30685281944005 1.0) (desc-length 64)
    (dirichlet-prior 0.693147180559947))
  ())
((0.041666666666666664
   (elem "root"
     (tr "left"
       (elem "gray"
         (tr "forward"
           (elem "gray" (tr "forward" (elem "red"))))))
     (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.041666666666666664
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray"
              (tr "forward"
                (elem "red" (tr "forward" (elem "red"))))))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.020833333333333332
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "blue" (tr "forward" (elem "blue")))))))))
# Grammar + top derivations:
(program
  ((abstraction F44 ()
     (choose (elem "gray" (tr "forward" (F1)))
       (elem "gray" (tr "forward" (F44)))
       (elem "gray" (tr "forward" (F41)))
       (elem "gray" (tr "forward" (F40)))))
    (abstraction F41 ()
      (choose (elem "blue")
        (elem "blue" (tr "forward" (F41)))))
    (abstraction F40 ()
      (choose (elem "red") (elem "red" (tr "forward" (F40)))))
    (abstraction F30 ()
      (elem "root" (tr "left" (F44)) (tr "right" (F44))))
    (abstraction F1 () (elem "gray" (tr "forward" (F40)))))
  (lambda () (choose (F30)))
  ((-1.3862943611198906 -1.3862943611198906
     -1.3862943611198906 -1.3862943611198906)
    (-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0)
    (0.0))
  (stats (posterior -73.06385622962489)
    (likelihood+weight -19.85561569885293 1.0)
    (prior+weight -53.208240530771945 1.0) (desc-length 55)
    (dirichlet-prior 1.7917594692280578))
  ())
((0.015625
   (elem "root"
     (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
     (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.015625
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.015625
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "red"))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.015625
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "red"))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.015625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))))
  (0.015625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.015625
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.015625
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "gray" (tr "forward" (elem "red"))))))))
  (0.015625
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.015625
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right" (elem "gray" (tr "forward" (elem "red")))))))
# Grammar + top derivations:
(program
  ((abstraction F45 ()
     (choose (elem "gray" (tr "forward" (F40)))
       (elem "gray" (tr "forward" (F45)))
       (elem "gray" (tr "forward" (F41)))))
    (abstraction F41 ()
      (choose (elem "blue")
        (elem "blue" (tr "forward" (F41)))))
    (abstraction F40 ()
      (choose (elem "red") (elem "red" (tr "forward" (F40)))))
    (abstraction F30 ()
      (elem "root" (tr "left" (F45)) (tr "right" (F45)))))
  (lambda () (choose (F30)))
  ((-1.0986122886681098 -1.0986122886681098
     -1.0986122886681098)
    (-0.6931471805599453 -0.6931471805599453)
    (-0.6931471805599453 -0.6931471805599453) (0.0) (0.0))
  (stats (posterior -64.10126823623841)
    (likelihood+weight -20.79441541679835 1.0)
    (prior+weight -43.30685281944005 1.0) (desc-length 44)
    (dirichlet-prior 0.6931471805599465))
  ())
((0.027777777777777776
   (elem "root"
     (tr "left" (elem "gray" (tr "forward" (elem "red"))))
     (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.027777777777777776
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "red"))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.027777777777777776
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.027777777777777776
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.027777777777777776
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "red"))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.027777777777777776
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "red"))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.027777777777777776
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right" (elem "gray" (tr "forward" (elem "red"))))))
  (0.027777777777777776
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "blue"))))
      (tr "right" (elem "gray" (tr "forward" (elem "blue"))))))
  (0.013888888888888888
    (elem "root"
      (tr "left" (elem "gray" (tr "forward" (elem "red"))))
      (tr "right"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))))
  (0.013888888888888888
    (elem "root"
      (tr "left"
        (elem "gray"
          (tr "forward"
            (elem "red" (tr "forward" (elem "red"))))))
      (tr "right" (elem "gray" (tr "forward" (elem "red")))))))
(#<void> #<void> #<void> #<void> #<void> #<void> #<void>
  #<void> #<void> #<void> #<void> #<void> #<void> #<void>
  #<void> #<void> #<void> #<void> #<void> #<void>)
