;; visualizing the simple examples
(library (write-boxes)
         (export 
           box-scene->graffle
           run-test)
(import (rnrs)
        (printing)
        (_srfi :1)
        (util))

(define
  transform-dirs
  '(("left" (-1 0))
    ("right" (1 0))
    ("forward" (0 1))
    ("red_chain" (-1 0))
    ("blue_chain" (1 0))
    ("reflect" h-reflect)
    ))

(define box-width 50)
(define box-height 30)

(define color-blue '(0 0.513653 1))
(define color-red '(1 0.086573 0))
(define color-gray '(0.514629 0.514629 0.514629))
(define color-other '(0.4 0.5 0.5))

(define
  elem-colors
  `(("red" ,color-red)
    ("other" ,color-other)
    ("root" ,color-gray)
    ("blue" ,color-blue)
    ("node" ,color-gray)
    ("tri" ,color-other)))

(define (mk-bounds-string x y w h) (string-append "{{" (number->string x) ","
                                                  (number->string y) "}}, {"
                                                  (number->string w) ","
                                                  (number->string h) "}"))

;; turns nesting of left/right/forward into the absolute coordinates
;; (elem "e1" (tr "left" (elem "e2" (tr "left" (elem "e3)))))
;; (coord '(0 0) "e1")
;; (coord '(-1 0) "e2")
;; (coord '(-2 0) "e3")

(define (vec-add xs ys)
  (map + xs ys))

(define (sg->abs-coord-list sg)
  (define coord-list '())
  (define (loop abs-coord is-reflected expr)
    (cond 
      [(null? expr) '()]
      [(eq? 'elem (car expr))
       (begin
         (set! coord-list (cons `(coord ,abs-coord ,(cadr expr) ,is-reflected) coord-list))
         (map (lambda (tr) (loop abs-coord is-reflected tr)) (cddr expr)))]
      [(eq? 'tr (car expr))
       (let* (
              [dx (cadr (assoc (cadr expr) transform-dirs))]
              [new-abs-coord (cond [(equal? 'h-reflect dx) abs-coord]
                                   [else (vec-add abs-coord dx)])]
              [new-is-reflect (cond [(equal? 'h-reflect dx) (not is-reflected)]
                                    [else is-reflected])])
         (loop new-abs-coord new-is-reflect (caddr expr)))]))
  (begin
    (loop '(0 0) #f sg)
    coord-list))

(define test
  '(elem "node"
         (tr "reflect"
             (elem "tri"
                   (tr "forward"
                       (elem "tri"))))
         (tr "left"
             (elem "tri"
                   (tr "forward"
                       (elem "red"))))))
(define (run-test)
  (pretty-print (box-scene->graffle test)))

(define global-counter 0)

(define (gen-id)
  (let* ([answer global-counter]
         [db (set! global-counter (+ 1 global-counter))])
    answer))

(define (coord-entry->dims coord-entry)
  (let* ([vec (cadr coord-entry)]
         [x (* box-width (car vec))]
         [y (* box-height (cadr vec))]
         [w box-width]
         [h box-height])
    (mk-bounds-string x y w h)))

(define (coord-entry->color coord-entry)
  (cadr (assoc (caddr coord-entry) elem-colors)))

(define (color->graffle-color rgb)
  `(dict
     (key Color)
     (dict
       (key r) (string ,(car rgb))
       (key g) (string ,(cadr rgb))
       (key b) (string ,(caddr rgb)))))

(define (elem-id->shape-type elem-id)
  (cond [(contains? elem-id '("node" "red" "blue" "other" "root")) 'Rectangle]
        [(equal? elem-id "tri") 'HorizontalTriangle]
        [else 'Rectangle]))

(define coord-entry->elem-id caddr)
(define coord-entry->reflected cadddr)

;; new format of coord-entry:
;; (coord abs-position elem-id is-reflected)
(define (mk-shape coord-entry)
  `(dict
     (key Bounds) (string ,(coord-entry->dims coord-entry))
     (key Class) (string ShapedGraphic)
     (key HFlip) (string ,(if (coord-entry->reflected coord-entry)
                            'YES 'NO))
     (key ID) (integer ,(gen-id))
     (key Shape) (string ,(elem-id->shape-type (coord-entry->elem-id coord-entry)))
     (key Style)
     (dict
       (key fill)
       ,(color->graffle-color (coord-entry->color coord-entry)))
     ))

(define (write-shapes shapes)
  (map
    (lambda (coord-entry)
      (mk-shape coord-entry))
    shapes))

(define (box-scene->graffle box-scene)
  `((plist (\x40; (version "1.0"))
                  (dict
                    (key GraphicsList)
                    (array
                      ,@(write-shapes (sg->abs-coord-list box-scene)))))))
)
