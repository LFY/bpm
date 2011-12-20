(import (_srfi :1))
(import (grammars))
(import (printing)
        (util))

(load (list-ref (command-line) 1))

(define (merge-diff i j)
  (grammar-diff
    (compactify (list-ref merge-history i))
    (compactify (list-ref merge-history j))))

(define all-merge-diffs
  (map (lambda (i) (list 'merge-number: i (merge-diff (+ 1 i) i)))
       (iota (- (length merge-history) 1))))

(define (pp-mergediff diff)
  (let* ([num (cadr diff)]
         [info (caddr diff)]
         [nts-before (car info)]
         [nts-after (caddr info)]
         [delta-stats (cdr (cadr (cadddr info)))])
    (begin
      (print "Merge: ~s to ~s" num (+ 1 num))
      (for-each pretty-print nts-before)
      (print "===>")
      (for-each pretty-print nts-after)
      (for-each
        (lambda (name-nums)
          (let* ([name (car name-nums)]
                 [nums (cdr name-nums)])
            (begin
              (display
                (apply (curry string-append (symbol->string name)
                               ": ") (map number->string nums))) (display " ")
              )))
        delta-stats)
      (newline))))

(for-each pp-mergediff all-merge-diffs)
