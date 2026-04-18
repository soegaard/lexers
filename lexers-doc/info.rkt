#lang info

(define collection 'multi)
(define pkg-desc "Scribble documentation for the lexers library.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base"
               "lexers-lib"
               "parser-tools-lib"
               "scribble-lib"
               "syntax-color-lib"))
(define build-deps '("parser-tools-doc"
                     "racket-doc"
                     "syntax-color-doc"))
(define scribblings '(("lexers.scrbl" () (library))))
