(library (forkmap)
         (export forkmap
                 forkmap-direct)
         (import (except (rnrs) delete-file)
                 (ikarus)
                 (_srfi :1)
                 (util)
                 (only (printing) print)
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


         ;; make-pipe author: Eduardo Cavazos
         ;; From discussion @
         ;; http://comments.gmane.org/gmane.lisp.scheme.ikarus.user/1554
         ;; (define (make-pipe . args)
         ;;   (let ((transcoder (if (pair? args)
         ;;                       (car args)
         ;;                       #f)))
         ;;     (let* ([fd (posix-pipe)]) 
         ;;       (vector (fh->input-port (vector-ref fd 0)
         ;;                               "pipe-in"
         ;;                               32768
         ;;                               transcoder
         ;;                               #t
         ;;                               'make-pipe)
         ;;               (fh->output-port (vector-ref fd 1)
         ;;                                "pipe-out"
         ;;                                32768
         ;;                                transcoder
         ;;                                #t
         ;;                                'make-pipe)))))


         ;; (define (forkmap-direct f xs)
         ;;   (define (id x) x)
         ;;   (let* ([num-threads (length xs)]
         ;;          [thread-ids (iota num-threads)]
         ;;          [all-pipes (map
         ;;                       (lambda (x)
         ;;                         (make-pipe (native-transcoder)))
         ;;                       (iota num-threads))]
         ;;          [all-pids (map
         ;;                      (lambda (pipe x tid)
         ;;                        (fork id
         ;;                              (lambda ()
         ;;                                (begin
         ;;                                  (with-output-to-port (vector-ref pipe 1) (lambda () (pretty-print (f x))))
         ;;                                  (exit 0)
         ;;                                  )
         ;;                                )))
         ;;                      all-pipes xs thread-ids)]
         ;;          )

         ;;       (let* ([answer
         ;;                (map (lambda (pipe tid)
         ;;              (let* (
         ;;                     [answer (read (vector-ref pipe 0))]
         ;;                     )
         ;;                (begin
         ;;                  
         ;;                  answer)))
         ;;            all-pipes thread-ids)])
         ;;     (begin 
         ;;       ;;(map (lambda (pid) (kill pid 'SIGKILL)) all-pids)
         ;;       answer))))

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
