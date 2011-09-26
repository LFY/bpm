(define string->goodXML
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")
     (#\' . "&apos;"))))

(define (entag tag elems)
  (cond
    ((and (pair? elems) (pair? (car elems))
	(eq? '@ (caar elems)))
      (let ((attrs (cdar elems)) (children (cdr elems)))
	(list #\< tag attrs 
	  (if (null? children) "/>\n"
	    (list #\> children "</" tag ">\n")))))
    ((null? elems) (list #\< tag "/>\n"))
    (else
      (list #\< tag #\> elems "</" tag ">\n"))))

(define (enattr attr-key value)
  (if (null? value) (list #\space attr-key "='" attr-key "'")
    (list #\space attr-key "='" value #\')))

(define (SXML->XML sxml)
  (define this-ss
    `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) (enattr attr-key value))))
      . ,(lambda (trigger . value) (cons '@ value)))
     (*default* . ,(lambda (tag . elems) (entag tag elems)))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodXML str) str)))
     (*TOP*       ; check for the namespaces and add xmlns:xxx attributes
      *macro*     ; to the root element
      . ,(lambda (tag . elems)
	   (define (make-xmlns ns-assoc)
	     (list (string->symbol
		    (string-append "xmlns:" (symbol->string (car ns-assoc))))
		   (cadr ns-assoc)))
	   (let*
	     ((namespaces ; extract from the annotations of *TOP*
	       (apply append
		 (pre-post-order elems
		   `((@
		       ((*NAMESPACES* *preorder*
			  . ,(lambda (tag . ns) ns))
			 (*default* *preorder* . ,(lambda x '())))
		       . ,(lambda (tag . elems) (apply append elems)))
		      (*default* *preorder* . ,(lambda x '()))))))
	       ;(_ (cerr "namespaces: " namespaces nl))
	       (xmlns-attrs (map make-xmlns namespaces)))
	     (pre-post-order elems
	       `((@ *preorder* . ,(lambda x '())) ; ignore *TOP* annotations
		  (*default*		; would handle the root elem
		    ((@ *preorder*	; attributes of the root element
		       . ,(lambda (tag . attrs) ; add xmlnsn attrs
			    (cons tag (append xmlns-attrs attrs))))
		     (*default* *preorder*
		       . ,(lambda x x))	; don't descend and don't transform
		      )
		    . ,(lambda (root-tag . children)
			 (if (and (pair? children) (pair? (car children))
			       (eq? '@ (caar children)))
			   (cons root-tag children)
			   ; root element had no attributes. Add xmlns ones
			   (cons* root-tag
			     `(@ ,xmlns-attrs) children))))))
	     )))
       ))
 (SRV:send-reply
  (pre-post-order sxml this-ss)
  ))

(define (my-filter pred lst)
  (cond
    ((null? lst) lst)
    ((pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))))
    (else (my-filter pred (cdr lst)))))

(define keep (lambda x x))
(define kill (lambda _ '()))
(define (postprocess tree)
  (pre-post-order tree
                  `(
                    (*default* . ,(lambda (tag . elts)
                                    `(,tag ,@(my-filter (lambda (x) (not (null? x))) elts))))
                    (*text* . ,(lambda (tag str) str))
                    )))

(define (main argv)

  (define (help)
    (for-each
     (lambda (docstring) (cerr docstring nl))
     docstrings)
    (exit 4))

  (if (not (= 4 (length argv)))
      (help))		; at least one argument, besides argv[0], is expected

  (let* ([original-dae (call-with-input-file (cadr argv) (lambda (port)
                                                           (ssax:xml->sxml port '[(dae . "http://www.collada.org/2005/11/COLLADASchema")])))]
         [myself (read (open-input-file (caddr argv)))]
         [spliced (postprocess (map (lambda (e) (pre-post-order e `( 
                                                  (*PI* . ,kill)
                                                  (*TOP* . ,keep)
                                                  (dae:COLLADA . ,keep)
                                                  (dae:library_visual_scenes
                                                    . ,(lambda elts `(dae:library_visual_scenes
                                                                     (dae:visual_scene
                                                                       (@ (id "GeneratedPlayground")
                                                                          (name "GeneratedPlayground"))
                                                                       ,myself))))
                                                  (*default* *preorder* . ,keep)
                                                  (*text* . ,(lambda (tag str) str)))))
                       original-dae))])


    (begin ;; (pp spliced)
           (with-output-to-file (cadddr argv) (lambda () (SXML->XML spliced)) 'replace)
           '())))
