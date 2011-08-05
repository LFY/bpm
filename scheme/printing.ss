(library (printing)
         (export display-all
                 println
                 printf
                 print
                 format
                 format-csv

                 str

                 delimit
                 delimit-format
                 delimit-with-formatter
                 
                 fold1
                 
                 )
         (import (rnrs)
                 (_srfi :1)
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

         (define (format-csv cols replacement-fx)
           (define cleaned-cols (filter (lambda (x) (not (eq? "" x))) 
                                        (map (lambda (xs) (map replacement-fx xs)) cols)))
           (define max-col-len (apply max (map length cleaned-cols)))

           (define (pad col)
             (cond [(eq? max-col-len (length col)) col]
                   [else (append col 
                                 (map (lambda (i) "")
                                      (iota (- max-col-len (length col)))))]))

           (define (print-one-row vals)
             (define (tab next acc)
               (string-append acc "\t" next))
             (fold1 tab vals))
           (define rows (apply zip (map pad cleaned-cols)))
           (string-append (delimit-with-formatter print-one-row "\n" rows)
                          "\n")
           )

         (define (str x)
           (format "~s" x))

         (define (fold1 acc xs)
           (fold acc (car xs) (cdr xs)))

         (define (delimit s xs)
           (fold1 (lambda (x y) (string-append y s x)) xs))

         (define (delimit-format s xs)
           (delimit s (map str xs)))

         (define (delimit-with-formatter f s xs)
           (delimit s (map f xs)))

         )