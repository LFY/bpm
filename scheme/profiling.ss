;; From http://phildawes.net/blog/2007/07/25/a-poor-mans-scheme-profiler/

(library (profiling)
         (export define-timed
                 current-profiling-stats
                 reset-timer)

         (import (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1)
                 (_srfi :69)
                 (_srfi :19)
                 (util))

         (define (time->float time) 
           (+ (time-second time) (/ (time-nanosecond time) (expt 10.0 9))))

         (define *timerhash* (make-hash-table equal?))

         ;;; call this before running the code
         (define (reset-timer) (set! *timerhash* (make-hash-table equal?)))

         ;;; adds the time spent in the thunk to an entry in the hashtable
         (define (accum-time name thunk)
           (let* ((timebefore (current-time))
                  (res (thunk)))
             (hash-table-set! *timerhash* name 
                              (let ((current (hash-table-ref *timerhash* name (lambda () '(0 0)))))
                                (list (+ (car current) (- (time->float (current-time)) (time->float timebefore)))
                                      (+ (cadr current) 1))))
             res))

         ;;; call this afterwards to get the times
         (define (current-profiling-stats)
           (map (lambda (e) (list (first e) 
                                  (* 1000 (second e))
                                  (third e)))

                (sort (lambda (a b) (> (cadr a) (cadr b))) (hash-table->alist *timerhash*)
                      )))

         (define-syntax define-timed
           (syntax-rules ()
                         ((define-timed (name . args) body ...)
                          (define (name . args)
                            (accum-time 'name (lambda ()
                                                body ...))))))

         )

