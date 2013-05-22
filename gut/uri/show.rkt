#lang typed/racket/base

(provide:
 [scheme->string (Scheme -> String)])

(require
 (only-in "types.rkt"
	  Scheme))

(: scheme->string (Scheme -> String))
(define (scheme->string scheme)
  (if (symbol? scheme)
      (string-downcase (symbol->string scheme))
      scheme))
