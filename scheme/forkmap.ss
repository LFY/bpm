(library (forkmap)
         (export forkmap)
         (import (except (rnrs) delete-file file-exists?)
                 (ikarus)
                 )

         (define (name-result n) (string-append "MyResult" (number->string n)))

         (define (read-all-results n)
           (define (loop acc k)
             (cond [(= k -1) acc]
                   [else (loop
                           (cons (with-input-from-file (name-result k) read) acc)
                           (- k 1))]))
           (loop '() n))

         (define (initialize-jobs)
           (system "rm MyResult*"))
       
         (define (fork-n-with-output n parent-fx par-fx)
           (cond [(= n 0) 
                  (fork parent-fx (lambda () (begin
                                               (par-fx 0)
                                               (exit 0))))]
                 [else
                   (fork parent-fx
                         (lambda () (begin
                                      (fork-n-with-output (- n 1) parent-fx par-fx)
                                      (par-fx n)
                                      (exit n))))]))

         (define all-pids '())
         (define (forkmap f xs)
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
               (read-all-results (- num-tasks 1)))))
         )
