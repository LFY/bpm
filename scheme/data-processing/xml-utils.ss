(library (xml-utils)
         (export pretty-print-xml)
         (import (rnrs)
                 (_srfi :1)
                 (printing)
                 (util))

         (define ssax-dir "/Users/lyang/bpm/scheme/SSAX/lib/")

         (define ssax-interp "echo \"(exit 101)\" | petite")

         (define ssax-deps (list "myenv-chez.scm"
                                 "srfi-13-local.scm"
                                 "char-encoding.scm"
                                 "util.scm"
                                 "look-for-str.scm"
                                 "input-parse.scm"
                                 "SSAX-code.scm"))

         (define run-sxml-script "/Users/lyang/bpm/scheme/data-processing/run-sxml.scm")
         (define run-sxml-cmd (delimit " " `(
                                             ,ssax-interp
                                             ,@(map (curry string-append ssax-dir) ssax-deps)
                                             ,run-sxml-script)))

         (define (make-run-sxml arg) 
           (format "`echo '(main (list \"myself\" ~s))' > /tmp/a.scm && echo /tmp/a.scm`" arg))

         (define (make-run-sxml-cmd arg)
           (delimit " " (list run-sxml-cmd (make-run-sxml arg))))

         (define (pretty-print-xml arg)
           (system (make-run-sxml-cmd arg))))
           
                                            
