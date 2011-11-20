(library (forkmap)
         (export forkmap)
         (import (except (rnrs) delete-file)
                 (ikarus)
                 (_srfi :1)
                 )

         (define (name-result n) (string-append "MyResult" (number->string n)))

         (define (read-all-results n)
           (define (loop acc k)
             (cond [(= k -1) acc]
                   [else (loop
                           (cons (with-input-from-file (name-result k) read) acc)
                           (- k 1))]))
           (loop '() n))

         (define all-pids '())
         (define (initialize-jobs)
           (begin
             (system "rm MyResult*")
             (set! all-pids '())
             ))
       
         (define (fork-n-with-output n parent-fx par-fx)
           (begin 
                  (cond [(= n 0) 
                         (fork parent-fx (lambda () (begin
                                                      (par-fx 0)
                                                      (exit 0))))]
                        [else
                          (fork parent-fx
                                (lambda () (begin
                                             (fork-n-with-output (- n 1) parent-fx par-fx)
                                             (par-fx n)
                                             (exit n))))])))


         (define (wait-for-files rem)
           (define (not-there? i) (not (file-exists? (name-result i))))
           (let* ([not-there-yet (filter not-there? rem)])
             (cond [(null? not-there-yet) '()]
                   [else (wait-for-files not-there-yet)])))
           
         (define (forkmap f xs)
           (cond [(null? xs) '()]
                 [else
                   (let* ([num-tasks (length xs)])
                     (begin
                       (initialize-jobs)
                       (fork-n-with-output 
                         (- num-tasks 1)
                         (lambda (pid)
                           (begin 
                             (set! all-pids (cons pid all-pids))
                             pid))
                         (lambda (i) 
                           (let* ([output-name (name-result i)])
                             (begin
                               (with-output-to-file output-name
                                                    (lambda ()
                                                      (display (f (list-ref xs i)))
                                                      ))))))
                       (for-each waitpid all-pids)
                       (wait-for-files (iota (- num-tasks 1)))
                       (read-all-results (- num-tasks 1))))]))
         )
