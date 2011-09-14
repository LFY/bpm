(import (printing)
        (delimcc-simple-ikarus))

;; This does not call continuation twice...
(define (test-shift e) (shift k (begin (print (k 1))
                                       (print (k 2)))))

(define (test-reset e) (reset `(a ,e)))

(pretty-print (test-reset `(node ,(test-shift 1))))

;; but this does
;;

(define (test-reset2 e) (reset `(a ,(e))))

(pretty-print (test-reset2 (lambda () `(node ,(test-shift 1)))))

