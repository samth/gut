#lang typed/racket/base

(provide
 links scripts stylesheets)

(require)

(define-syntax stylesheets
  (syntax-rules ()
    ((_ alink ...)
     '((link (@ (rel "stylesheet") (type "text/css") (href alink)) "")...))))

(define-syntax links
  (syntax-rules ()
    ((_ alink ...)
     '((link (@ (rel "stylesheet") (type "text/css") (href alink)) "")...))))

(define-syntax scripts
  (syntax-rules ()
    ((_ ascript ...)
     `((script (@ (type "text/javascript") (src ascript)) "")...))))

