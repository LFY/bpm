(library (printing)
         (export display-all
                 println
                 printf
                 print
                 format)
         (import (rnrs)
                 (srfi :48))


         (define (display-all . args)
           (for-each display args))

         (define (println . xs)
           (begin
             (for-each display xs)
             (display "\n")))

         (define (printf fmt . xs)
           (display (apply format (cons fmt xs))))

         (define (print fmt . xs)
           (if (string? fmt)
             (begin
               (apply printf (cons fmt xs))
               (display "\n"))
             (begin
               (apply display-all (cons fmt xs))
               (display "\n"))))

         )
