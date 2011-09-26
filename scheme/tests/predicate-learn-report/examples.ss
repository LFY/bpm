program: 
(let ()
  (define F1
    (lambda (V0 V1 V2 V3)
      (node (data (A0 V0) (A1 V1) (A2 V2) (A3 V3)))))
  (lambda ()
    (node (data) (F1 1 -1 2 -2) (F1 2 -2 3 -3)
      (F1 4 -4 5 -5) (F1 7 -7 8 -8) (F1 9 -9 10 -10))))
size (used in prior): 38

after functional deargumentation:
(let ()
  (define F1
    (lambda (V0)
      (node
        (data (A0 V0) (A1 (- V0)) (A2 (+ V0 1))
          (A3 (- (+ V0 1)))))))
  (lambda ()
    (node (data) (F1 1) (F1 2) (F1 4) (F1 7) (F1 9))))
size (used in prior): 29

program: 
(let ()
  (define F1 (lambda (V0 V1) (node (data (A0 V0)) V1)))
  (lambda ()
    (node (data)
      (F1 1 (F1 2 (F1 3 (F1 4 (node (data (A0 5))))))))))
size (used in prior): 20

after functional deargumentation:
(let ()
  (define F1
    (lambda (V0)
      ((lambda (V1) (node (data (A0 V0)) V1))
        (if (flip 3/4)
            (F1 (+ V0 1))
            (uniform-choice (node (data (A0 5))))))))
  (lambda () (node (data) (F1 1))))
size (used in prior): 24

program: 
(let ()
  (define F1 (lambda (V0 V1) (node (data (A0 V0)) V1)))
  (lambda ()
    (node (data) (F1 2 (F1 3 (node (data (A0 4)))))
      (node (data)
        (F1 1 (F1 2 (F1 3 (F1 4 (node (data (A0 5)))))))
        (F1 3 (F1 4 (F1 5 (F1 6 (node (data (A0 7))))))))
      (F1 5 (F1 6 (node (data (A0 6))))))))
size (used in prior): 50

after functional deargumentation:
(let ()
  (define F1
    (lambda (V0)
      ((lambda (V1) (node (data (A0 V0)) V1))
        (if (flip 2/3)
            (F1 (+ V0 1))
            (uniform-choice (node (data (A0 4)))
              (node (data (A0 5))) (node (data (A0 7)))
              (node (data (A0 6))))))))
  (lambda ()
    (node (data) (F1 2) (node (data) (F1 1) (F1 3)) (F1 5))))
size (used in prior): 44

program: 
(let ()
  (define F1
    (lambda (V0 V1 V2) (node (data (A0 V0) (A1 V1)) V2)))
  (lambda ()
    (node (data)
      (F1 1 3
        (F1 2 4 (F1 3 5 (F1 4 6 (node (data (A0 5))))))))))
size (used in prior): 26

after functional deargumentation:
(let ()
  (define F1
    (lambda (V0)
      ((lambda (V2) (node (data (A0 V0) (A1 (+ V0 2))) V2))
        (if (flip 3/4)
            (F1 (+ V0 1))
            (uniform-choice (node (data (A0 5))))))))
  (lambda () (node (data) (F1 1))))
size (used in prior): 28