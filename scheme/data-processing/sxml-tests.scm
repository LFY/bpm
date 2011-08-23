;
; $Id: run-sxml.scm,v 1.2 2002/12/10 22:28:14 oleg Exp $

(define docstrings
 '(
 " Transform an XML document into SXML. See ../docs/SXML.html for description."
 ""
 " Usage"
 "	xml-to-sxml xml-file-name"
 " The translated SXML document is printed to the standard output."
))

(define (process port)
  (let* ([doc2 (ssax:xml->sxml port '())]
         ;; [doc2 `(*TOP2* (@) ,@doc)]
         ;; [db (pp (car doc))]
         ;; [db (pp (cadr doc))]
         ;; [db (pp "debug over")]
         )
    ([sxpath '(doc title)]
     '(*TOP*
        (doc (title "Hello world")))
     )
    ))
          ;;(postprocess (simplify-bento (ssax:xml->sxml port '()))))

(define (main argv)

  (define (help)
    (for-each
     (lambda (docstring) (cerr docstring nl))
     docstrings)
    (exit 4))

  (if (not (= 2 (length argv)))
      (help))		; at least one argument, besides argv[0], is expected
  (pp 
   (call-with-input-file (cadr argv)
     process))
)


