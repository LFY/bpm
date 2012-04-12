(library (node-constructors)
         (export node
                 data
                 a0
                 a1
                 a2
                 a3

                 p

                 NULL HTML BODY DIV A UL LI H1 H2 P
                 name left top width height
                 x y

                 gridline-pass
                 gridline-coord
                 figure
                 box
                 circle
                 filledCircle
                 polygon
                 filledPolygon
                 polyline
                 polylineRound

                 vec
                 model
                 sphere
                 cube
                 cylinder
                 ring

                 transArray
                 rotArray
                 arcArray
                 refArray

                 scale
                 translate
                 rotate
                 reflect
                 flip

                 intersection
                 union
                 difference

                 asm

                 pos
                 bbox_min
                 bbox_max
                 color
                 radius
                 fillcolor
                 points
                 dashed
                 smooth
                 stroke_linecap
                 stroke
                 local_scale
                 base
                 inner_base
                 outer_base

                 order
                 offset
                 axis
                 dt
                 start
                 amount
                 filename

                 gaussian
                 size

                 define-constr

                 fine_print
contact
footer
text_link
text
image
heading
container
threeColumn
headline
hero_image
navigation_element
logo
navigation
page_root
copyright
twoColumn
header
auxiliary_navigation
social_media
subheading
search
login
region
image_link
fourColumn
form
shopping_links
sitemap
transaction_icons
auxiliary_navigtion
pagination
                 )
         (import (rnrs)
                 (_srfi :1)
                 (printing))

         (define (node attrs . children) (append (list 'node attrs) children))
         (define (data . attrs) (append (list 'data) attrs))

         (define (a0 x) (list 'a0 x))
         (define (a1 x) (list 'a1 x))
         (define (a2 x) (list 'a2 x))
         (define (a3 x) (list 'a3 x))

         (define (p . xs) (append (list 'p) xs))

         ;; HTML stuff
         (define NULL 'NULL)
         (define HTML 'HTML)
         (define BODY 'BODY)
         (define DIV 'DIV)
         (define A 'A)
         (define UL 'UL)
         (define LI 'LI)
         (define H1 'H1)
         (define H2 'H2)
         (define P 'P)

         (define (left x) `(left ,x))
         (define (top x) `(top ,x))
         (define (width x) `(width ,x))
         (define (height x) `(height ,x))
         (define (name x) `(name ,x))
         (define (x z) `(x ,z))
         (define (y z) `(y ,z))

         ;; Gridline drawing

         (define (gridline-pass x) x)
         (define (gridline-coord tag x)
           (cond [(null? x) '()]
                 [else (begin (print "(~s ~s)" tag x)
                              x)]))

         (define-syntax define-constr
           (syntax-rules ()
                         [(define-constr name)
                          (define (name . xs)
                            (cons 'name xs))]))
         ;; pyscenegraph

         (define-constr vec)
         (define-constr figure)
         (define-constr box)
         (define-constr circle)
         (define-constr filledCircle)
         (define-constr polygon)
         (define-constr filledPolygon)
         (define-constr polyline)
         (define-constr polylineRound)

         (define-constr model)
         (define-constr sphere)
         (define-constr cube)
         (define-constr cylinder)
         (define-constr ring)

         (define-constr transArray)
         (define-constr rotArray)
         (define-constr arcArray)
         (define-constr refArray)

         (define-constr scale)
         (define-constr translate)
         (define-constr rotate)
         (define-constr reflect)
         (define-constr flip)

         (define-constr intersection)
         (define-constr union)
         (define-constr difference)

         (define-constr asm)

         ;; sub-constructors, attributes
         (define-constr pos)
         (define-constr bbox_min)
         (define-constr bbox_max)
         (define-constr color)
         (define-constr radius)
         (define-constr fillcolor)
         (define-constr stroke)
         (define-constr points)
         (define-constr dashed)
         (define-constr smooth)
         (define-constr stroke_linecap)
         (define-constr local_scale)
         (define-constr base)
         (define-constr inner_base)
         (define-constr outer_base)

         (define-constr order)
         (define-constr offset)
         (define-constr axis)
         (define-constr dt)
         (define-constr start)
         (define-constr amount)

         (define-constr filename)

         (define-constr gaussian)
         (define-constr size)

(define-constr fine_print)
(define-constr contact)
(define-constr footer)
(define-constr text_link)
(define-constr text)
(define-constr image)
(define-constr heading)
(define-constr container)
(define-constr threeColumn)
(define-constr headline)
(define-constr hero_image)
(define-constr navigation_element)
(define-constr logo)
(define-constr navigation)
(define-constr page_root)
(define-constr copyright)
(define-constr twoColumn)
(define-constr header)
(define-constr auxiliary_navigation)
(define-constr social_media)
(define-constr subheading)
(define-constr search)
(define-constr login)
(define-constr region)
(define-constr image_link)
(define-constr fourColumn)
(define-constr form)
(define-constr shopping_links)
(define-constr sitemap)
(define-constr transaction_icons)
(define-constr auxiliary_navigtion)
(define-constr pagination)
         )
