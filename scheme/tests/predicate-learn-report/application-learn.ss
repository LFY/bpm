before transform:
(let ()
  (define F1
    (lambda (V0 V1 V2 V3)
      (node (data (A0 V0) (A1 V1) (A2 V2) (A3 V3)))))
  (lambda ()
    (node (data) (F1 1 2 3 4) (F1 2 3 4 5) (F1 3 4 5 6)
      (F1 6 7 8 9) (F1 10 11 12 13))))
size (used in prior): 38

after transform:
(let ()
  (define F1
    (lambda (V0)
      (node
        (data (A0 V0) (A1 (+ V0 1)) (A2 (+ V0 2))
          (A3 (+ V0 3))))))
  (lambda ()
    (node (data) (F1 1) (F1 2) (F1 3) (F1 6) (F1 10))))
size (used in prior): 29

before transform:
(let ()
  (define F1
    (lambda (V0 V1 V2 V3 V4)
      (node (data (A0 V0) (A1 V1) (A2 V2) (A3 V3) (A4 V4)))))
  (lambda ()
    (node (data) (F1 1 2 3 4 5) (F1 3 4 5 6 7)
      (F1 4 5 6 7 8))))
size (used in prior): 33

after transform:
(let ()
  (define F1
    (lambda (V0)
      (node
        (data (A0 V0) (A1 (+ V0 1)) (A2 (+ V0 2))
          (A3 (+ V0 3)) (A4 (+ (+ V0 3) 1))))))
  (lambda () (node (data) (F1 1) (F1 3) (F1 4))))
size (used in prior): 31


before transform:
(let ()
  (define F1
    (lambda (V0 V1 V2 V3)
      (node (data (A0 V0) (A1 V1) (A2 V2) (A3 V3)))))
  (lambda ()
    (node (data) (F1 1 -1 2 -2) (F1 2 -2 3 -3)
      (F1 4 -4 5 -5) (F1 7 -7 8 -8) (F1 9 -9 10 -10))))
size (used in prior): 38

after transform:
(let ()
  (define F1
    (lambda (V0)
      (node
        (data (A0 V0) (A1 (- V0)) (A2 (+ V0 1))
          (A3 (- (+ V0 1)))))))
  (lambda ()
    (node (data) (F1 1) (F1 2) (F1 4) (F1 7) (F1 9))))
size (used in prior): 29

before transform:
(let ()
  (define F1
    (lambda (V0 V1 V2 V3)
      (node (data (A0 V0) (A1 V1) (A2 V2) (A3 V3)))))
  (lambda ()
    (node (data) (F1 0 2 4 6) (F1 3 5 7 9) (F1 5 7 9 11)
      (F1 10 12 14 16))))
size (used in prior): 33

after transform:
(let ()
  (define F1
    (lambda (V0 V1 V2)
      (node (data (A0 V0) (A1 V1) (A2 V2) (A3 (+ V2 2))))))
  (lambda ()
    (node (data) (F1 0 2 4) (F1 3 5 7) (F1 5 7 9)
      (F1 10 12 14))))
size (used in prior): 31


before transform:
(let ()
  (define F1
    (lambda (V0 V1 V2) (node (data (A0 V0) (A1 V1) (A2 V2)))))
  (lambda ()
    (node (data) (F1 1 2 3) (F1 2 4 5) (F1 3 6 7)
      (F1 6 12 13))))
size (used in prior): 27

after transform:
(let ()
  (define F1
    (lambda (V0 V2)
      (node (data (A0 V0) (A1 (* 2 V0)) (A2 V2)))))
  (lambda ()
    (node (data) (F1 1 3) (F1 2 5) (F1 3 7) (F1 6 13))))
size (used in prior): 25



