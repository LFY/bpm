(import (_srfi :1))
(import (grammars))
(import (printing))

(load (list-ref (command-line) 1))

(define (merge-diff i j)
  (grammar-diff
    (compactify (list-ref merge-history i))
    (compactify (list-ref merge-history j))))

(define all-merge-diffs
  (map (lambda (i) (list 'merge-number: i (merge-diff (+ 1 i) i)))
       (iota (- (length merge-history) 1))))

(for-each pretty-print all-merge-diffs)
