#lang scribble/doc

@begin[(require scribble/manual		
		scribble/base)
       (require (for-label (only-meta-in 0 typed/racket)))]

@title[#:tag "top"]{@bold{HTTPClient} A Typed Racket HTTP Client}

@declare-exporting["../http11.rkt"]

@table-of-contents[]

by Ray Racine (@tt{ray dot racine at gmail dot com})

This library provides a synchronous invoking HTTP Client which is generally in accordance with HTTP 1.1 supporting gzip compression, HTTPS support and chunked encoding.

@include-section["http.scrbl"]

