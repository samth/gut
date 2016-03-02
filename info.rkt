#lang setup/infotab
(define deps (list "base"
                   "srfi-lite-lib"
                   "sxml"
                   "typed-racket-lib"
                   "typed-racket-more"
                   "grip" "grommet" "html-parsing" "html-writing" "json-parsing"))
(define collection 'multi)
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "typed-racket-doc"))
