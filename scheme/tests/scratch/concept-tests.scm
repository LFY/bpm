Each concept is tested with 20 examples.

concept:
(abstraction F1 (V0)
  (data (A0 V0) (A1 (+ V0 1)) (A2 (+ V0 2)) (A3 (+ V0 3))))

learned functions:

variance = 0.0:
(abstraction F1 (V0)
  (node
    (data (A0 V0) (A1 (+ V0 1)) (A2 (+ V0 2.0))
      (A3 (+ V0 3.0)))))

variance = 0.05:
(abstraction F1 (V0)
  (node
    (data (A0 V0) (A1 (+ V0 1)) (A2 (+ V0 2.00787244288228))
      (A3 (+ V0 2.999844059037199)))))

variance = 0.1:
(abstraction F1 (V0)
  (node
    (data (A0 V0) (A1 (+ V0 1))
      (A2 (+ V0 2.0211551782014863))
      (A3 (+ V0 3.012709338720263)))))

variance = 0.2:
(abstraction F1 (V0 V1)
  (node
    (data (A0 V0) (A1 V1) (A2 (+ V1 0.8722332315179534))
      (A3 (+ V1 1.869198110695582)))))

variance = 0.5 and above:
(abstraction F1 (V0 V1 V2 V3)
  (node (data (A0 V0) (A1 V1) (A2 V2) (A3 V3))))

concept:
(abstraction F1 (V0)
  (let* ((V1 (if (flip) (F1 (+ V0 1)) (node (data A0 0)))))
    (node (data (A0 V0) (A1 (+ V0 2))) V1)))

learned functions:

variance = 0.0:
(abstraction F1 (V0)
  ((lambda (V2)
     (node (data (A0 V0) (A1 (+ V0 2.0)) (A2 V2)) V3))
    (if (flip 9/10)
        (F1 (+ V0 1))
        <uniform choices over endings>)))

variance = 0.05:
(abstraction F1 (V0)
  ((lambda (V2)
     (node
       (data (A0 V0) (A1 (+ V0 1.996870691680549)) (A2 V2))
       V3))
    (if (flip 9/10)
        (F1 (+ V0 1))
        <uniform choices over endings>)))

variance = 0.1:
(abstraction F1 (V0 V1)
  ((lambda (V2) (node (data (A0 V0) (A1 V1) (A2 V2)) V3))
    (if (flip 9/10)
        (F1 (+ V0 1) (+ V1 1))
        <uniform choices over endings>)))

variance = 0.2:
(abstraction F1 (V0 V1)
  ((lambda (V2) (node (data (A0 V0) (A1 V1) (A2 V2)) V3))
    (if (flip 9/10)
        (F1 (+ V0 1) V1)
        <uniform choices over endings>)))

variance = 0.5 and above:
(abstraction F1 (V0 V1)
  ((lambda (V2) (node (data (A0 V0) (A1 V1) (A2 V2)) V3))
    (if (flip 9/10)
        (F1 V0 V1)
        <uniform choices over endings>)))

concept:
(abstraction F1 (V0)
  (let* ((V1 (if (flip) (F1 (+ V0 1)) (node (data A0 0)))))
    (node (data (A0 V0)) V1)))

learned functions:

variance = 0.0:
(abstraction F1 (V0)
  ((lambda (V1) (node (data (A0 V0) (A1 V1)) V2))
    (if (flip 4/5)
        (F1 (+ V0 1))
        <uniform choices over endings>)))

variance = 0.05:
(abstraction F1 (V0)
  ((lambda (V1) (node (data (A0 V0) (A1 V1)) V2))
    (if (flip 4/5)
        (F1 (+ V0 1))
        <uniform choices over endings>)))

variance = 0.1:
(abstraction F1 (V0)
  ((lambda (V1) (node (data (A0 V0) (A1 V1)) V2))
    (if (flip 4/5)
        (F1 (+ V0 1))
        <uniform choices over endings>)))

variance = 0.2:
(abstraction F1 (V0)
  ((lambda (V1) (node (data (A0 V0) (A1 V1)) V2))
    (if (flip 4/5)
        (F1 (+ V0 1))
        <uniform choices over endings>)))

variance = 0.5 and above:
(abstraction F1 (V0)
  ((lambda (V1) (node (data (A0 V0) (A1 V1)) V2))
    (if (flip 4/5)
        (F1 V0)
        <uniform choices over endings>)))

concept:
(abstraction F1 (V0)
  (let* ((V1 (if (flip) (F1 (* V0 -1)) (node (data A0 0)))))
    (node (data (A0 V0)) V1)))

learned functions:

variance = 0.0:
(abstraction F1 (V0)
  ((lambda (V1) (node (data (A0 V0) (A1 V1)) V2))
    (if (flip 4/5)
        (F1 (- V0))
        <uniform choices over endings>)))

variance = 0.05 and above:
(abstraction F1 (V0)
  ((lambda (V1) (node (data (A0 V0) (A1 V1)) V2))
    (if (flip 4/5)
        (F1 V0)
        <uniform choices over endings>)))

concept:
(abstraction F1 (V0)
  (node
    (data (A0 V0) (A1 (- V0)) (A2 (+ V0 1))
      (A3 (- (+ V0 1))))))
learned functions:

variance = 0.0:
(abstraction F1 (V0)
  (node
    (data (A0 V0) (A1 (- V0)) (A2 (+ V0 1.0))
      (A3 (- (+ V0 1.0))))))

variance = 0.05:
(abstraction F1 (V0 V1 V3)
  (node (data (A0 V0) (A1 V1) (A2 (+ V0 1)) (A3 V3))))

variance = 0.1:
(abstraction F1 (V0 V1 V3)
  (node (data (A0 V0) (A1 V1) (A2 (+ V0 1)) (A3 V3))))

variance = 0.2:
(abstraction F1 (V0 V1 V3)
  (node (data (A0 V0) (A1 V1) (A2 (+ V0 1)) (A3 V3))))

variance = 0.5 and above:
(abstraction F1 (V0 V1 V2 V3)
  (node (data (A0 V0) (A1 V1) (A2 V2) (A3 V3))))

