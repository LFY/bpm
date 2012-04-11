(import (mcmc)
        (grammar-proposals)
        (grammar-likelihood)
        (grammar-induction)
        (grammars)
        (util)
        (printing)
        (_srfi :1)
        (srfi :13)
        (scene-graphs))

;; Functions to print grammar and stats per iteration===========================

(define (print-grammar-stats grammar output-port)
  (let* ([stats (cdr (grammar->stats grammar))]
         [relevant (filter (lambda (stat)
                             (contains? (car stat)
                                        '(posterior likelihood+weight prior+weight desc-length dirichlet-prior)))
                           stats)]
         [format-one (lambda (stat)
                       (delimit " " (map (lambda (x) (cond [(number? x) (number->string x)]
                                                           [(symbol? x) (symbol->string x)]
                                                           [else x])) stat)))]
         [formatted-lines (map format-one relevant)])
    (display (string-append (delimit "\n" formatted-lines) "\n") output-port)))

(define (print-best-score output-port)
  (define best-score '())
  (define iter 0)
  (lambda (state)
    (let* ([accept? (list-ref state 5)]
           [next-state (list-ref state 2)]
           [next-state-score (grammar->grammar+posterior test-data next-state likelihood-weight prior-weight dirichlet-alpha)]
           [next-state (car next-state-score)]
           [next-score (cadr next-state-score)]
           )
      (begin
        (if (and accept? (or (null? best-score) (> next-score best-score)))
          (begin
            (set! best-score next-score)
            (set! best-state next-state)
            (set! iter (+ 1 iter)))
          (begin
            (set! iter (+ 1 iter))))
        (pretty-print 
          `(define ,(string->symbol (string-append "grammar-iter" (number->string iter))) (quote ,best-state))
          output-port)
        (print-grammar-stats best-state output-port))
      )))


;; Parse arguments (exemplar file, fan-out, iteration count)====================

(define (extract-output-name x)
  (car (string-tokenize
    (list->string
      (map (lambda (x)
             (cond [(eq? x #\.) #\space]
                   [else x]))
           (string->list x))))))

(define (opt-select xs idx val)
  (if (<= (length xs) idx) val (list-ref xs idx)))

(define argv (command-line))
(define dir-to-load (list-ref argv 1))
(define file-to-load "exemplars.ss")
(define abs-file-to-load (string-append dir-to-load "/" file-to-load))

(define fan-out (string->number (opt-select argv 2 "10")))
(define num-iter (string->number (opt-select argv 3 "100")))
(define likelihood-weight (string->number (opt-select argv 4 "1.0")))
(define prior-weight (string->number (opt-select argv 5 "1.1")))
(define dirichlet-alpha (string->number (opt-select argv 6 "0.8")))
(define model-scale (string->number (opt-select argv 7 "1.0")))
(define trial (opt-select argv 8 '()))

(define log-filename "run.log")
(define grammar-filename "grammar.ss")

;; handling output directory/log format=============================================

(define scale model-scale)
(define beam fan-out)
(define likeprior (/ likelihood-weight prior-weight))
(define alpha dirichlet-alpha)
(define stop num-iter)
(define strategy 7)
(define trial-num trial)

(define param-string (delimit "_" (cons "lgcg" (filter (lambda (x) (not (null? x)))
                                                        (append (map number->string (list scale beam))
                                                                (list (fixed-format 4 likeprior))
                                                                (map number->string (list alpha stop strategy))
                                                                (list trial))))))
(define output-dir-name (string-append dir-to-load "/" param-string))

(define rel-log-filename (string-append output-dir-name "/" (string-append dir-to-load "_" param-string ".log")))
(define rel-grammar-filename (string-append output-dir-name "/" (string-append dir-to-load "_" param-string ".grammar.ss")))

;; Initial loading of exemplars and output dir setup============================

(load abs-file-to-load)

(system (format "mkdir -p ~s" output-dir-name))
(system (format "rm ~s" rel-log-filename))
(define fh (open-output-file rel-log-filename))

;; Distinguish between webpages and 3D models

(define (proc-model? exprs)
  (equal? 'elem (caar exprs))) ;; suffices for now...

(define (bento->grammar bento-nodes)
  (define (strip-ids t)
    (cond [(null? t) '()]
          [else
            (cons (car t) (map strip-ids (cddr t)))]))
  (let* ([no-ids (map strip-ids bento-nodes)])
    (lgcg-generic no-ids (lambda (e) (and (list? e) (symbol? (car e)))))))

(define best-state 
  (if (proc-model? test-data)
    (lgcg test-data)
    (bento->grammar test-data)))

;; Running the algorithm========================================================

(define best-gr (car (grammar->grammar+posterior test-data 
                                                 (if (null? best-state) initial-gr best-state) 
                                                 likelihood-weight prior-weight dirichlet-alpha)))

(pretty-print `(define ,(string->symbol (string-append "grammar-iter" (number->string 0))) (quote ,best-gr)) fh)
(print-grammar-stats best-gr fh)

(system (format "rm ~s" rel-grammar-filename))

(define gr-fh (open-output-file rel-grammar-filename))

(pretty-print `(define grammar (quote ,best-gr)) gr-fh)
(pretty-print `(define transforms (quote ,transforms)) gr-fh)

