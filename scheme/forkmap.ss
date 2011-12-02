(library (forkmap)
         (export forkmap
                 forkmap-direct)
         (import (except (rnrs) delete-file)
                 (ikarus)
                 (_srfi :1)
                 (util)
                 (only (printing) pretty-print)
                 )

         (define (name-result n) (string-append "MyResult" (number->string n)))
         (define (done-result n) (string-append "MyResultDone" (number->string n)))
         (define (name-dir n) (string-append "MyResultDir" (number->string n)))

         (define (read-all-results n)
           (define (loop acc k)
             (cond [(= k -1) acc]
                   [else (loop
                           (cons (with-input-from-file (name-result k) read) acc)
                           (- k 1))]))
           (loop '() n))

         (define all-pids '())
         (define (clean-files)
           (system "rm MyResult*"))
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
                                             ;; (system (string-append "mkdir " (name-dir n)))
                                             ;; (system (string-append "cd " (name-dir n)))
                                             (par-fx n)
                                             (exit n))))])))


         (define (wait-for-files rem)
           (define (not-there? i) (not (file-exists? (done-result i))))
           (let* ([not-there-yet (filter not-there? rem)])
             (cond [(null? not-there-yet) '()]
                   [else (wait-for-files not-there-yet)])))
           
         (define (forkmap-direct f xs)
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
                                                      (pretty-print (f (list-ref xs i)))
                                                      ))
                               (system (string-append "touch " (done-result i)))
                               ))))
                       (for-each waitpid all-pids)
                       (wait-for-files (iota (- num-tasks 1)))
                       (read-all-results (- num-tasks 1))))
                   
                   
                   ]))


         (define-opt (forkmap f xs (optional 
                                 (num-threads 8)))
           (cond [(null? xs) '()]
                 [else
                   (let* (
                          [subtasks (split-into num-threads xs)]
                          [num-tasks (length subtasks)])
                     (concatenate
                       (forkmap-direct (lambda (sub-group)
                                (map f sub-group))
                              subtasks)))]))

         )
