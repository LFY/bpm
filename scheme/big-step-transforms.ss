(library (big-step-transforms)
         (export inverse-inline-query-transforms)
         (import (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1)
                 (program)
                 (inverse-inline)
                 (query-learn)
                 (sym)
                 (util))


         (define (inverse-inline-query-transforms prog . nofilter)
           (let* ([inverse-inline-candidates (apply (curry compressions prog) nofilter)]
                  [query-transform-candidates (concatenate (map arith-dearguments inverse-inline-candidates))]
                  [prog-size (program-size prog)]
                  [valid-compressed-programs
                    (if (not (null? nofilter))
                      query-transform-candidates
                      (filter (lambda (cp) (< (program-size cp)
                                              prog-size))
                              query-transform-candidates))])
             valid-compressed-programs))



         )

