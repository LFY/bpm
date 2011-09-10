(library (html-vis)
         (export html-program->gridlines
                 )
         (import (rnrs)
                 (rnrs eval)
                 (program)
                 (util)
                 (_srfi :1)
                 (printing)
                 (church external py-pickle))

         (define (html-program->gridlines prog)

           (define (rewrite-body body)
             (subexpr-walk (lambda (t) (cond [(eq? 'x (car t)) (replace-car t `(lambda (c) (gridline-coord 'draw-x c)))]
                                             [(eq? 'y (car t)) (replace-car t `(lambda (c) (gridline-coord 'draw-y c)))]
                                             [(eq? 'width (car t)) (replace-car t `(lambda (c) (gridline-coord 'draw-width c)))]
                                             [(eq? 'height (car t)) (replace-car t `(lambda (c) (gridline-coord 'draw-height c)))]

                                             [(eq? 'choose (car t)) (quote '())]

                                             ;; [(eq? 'node (car t)) (replace-car t 'gridline-pass)]
                                             ;; [(eq? 'data (car t)) (replace-car t 'gridline-pass)]
                                             ;; [(eq? 'width (car t)) (replace-car t 'gridline-pass)]
                                             ;; [(eq? 'height (car t)) (replace-car t 'gridline-pass)]
                                             [else t]))
                           body))

           (define (transform-abstr abstr)
             (make-named-abstraction (abstraction->name abstr)
                                     (rewrite-body (abstraction->pattern abstr))
                                     (abstraction->vars abstr)))

           (define (replace-var-with-null bound-vars body)
             (subexpr-walk (lambda (t) (cond [(eq? 'choose (car t)) (quote '())]
                                             [(list? t) (map (lambda (x) (cond [(contains? x bound-vars) (quote '())]
                                                                               [else x])) t)]
                                             [else t]))
                           body))

           (let* ([abstrs (program->abstractions prog)]
                  [new-abstrs (map transform-abstr abstrs)]
                  [generated-applications (map (lambda (vars-pattern) (replace-var-with-null (first vars-pattern) (second vars-pattern)))
                                               (zip (map abstraction->vars new-abstrs)
                                                    (map abstraction->pattern new-abstrs)))]
                  [new-body `(lambda () (with-input-from-string (with-output-to-string (lambda () (begin (print "(")
                                                                                                         ,@generated-applications
                                                                                                         (print ")")))) read))]
                  ;; [new-body `(lambda () (begin ,@generated-applications))]
                  [new-program (make-program new-abstrs new-body)])
             (begin (pretty-print new-program)
                    (draw-gridlines ((eval (program->sexpr new-program) 
                                           (environment '(rnrs) 
                                                        '(node-constructors) 
                                                        '(printing)
                                                        '(only (ikarus) 
                                                               with-output-to-string
                                                               with-input-from-string
                                                               ))))))))

         (define (draw-gridlines grid-cmds)

           (define (extract-by tag)
             (map cadr (filter (lambda (tag-coord) (eq? tag (car tag-coord))) grid-cmds)))

           (define (get-bbox xs ys)
             (let* ([xs* (if (null? xs) '(0) xs)]
                    [ys* (if (null? ys) '(0) ys)])
               (list (list (apply min xs*) (apply min ys*))
                     (list (apply max xs*) (apply max ys*)))))

           (define bbox->minx caar)
           (define bbox->miny cadar)
           (define bbox->maxx caadr)
           (define bbox->maxy cadadr)

           (define (pad-bbox pad bbox)
             (list (list (- (bbox->minx bbox) pad) (- (bbox->miny bbox) pad))
                   (list (+ (bbox->maxx bbox) pad) (+ (bbox->maxy bbox) pad))))

           (let* ([x-coords (extract-by 'draw-x)]
                  [y-coords (extract-by 'draw-y)]
                  [bbox (pad-bbox 50 (get-bbox x-coords y-coords))])
             ((py-pickle-function "draw_gridlines.py") (list x-coords y-coords bbox))))

         )
