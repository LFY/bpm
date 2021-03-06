(library (python-fg-lib)
         (export draw-trees system)
         (import (except (rnrs) string-hash string-ci-hash)
                 (scheme-tools py-pickle)
                 (util)
                 ;(church external py-pickle)
                 )
         (define py-dir "/Users/lyang/store/Dropbox/bpm/python/")

         (define system
           (py-pickle-script (string-append py-dir "system.py")))

         (define draw-trees
           (py-pickle-script (string-append py-dir "treedraw.py")))

         (define entails?
           (py-pickle-function (string-append py-dir "entails.py")))


         
         )
