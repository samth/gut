;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide 
 Sxml
 SXPath sxpath
 html->sxml sxml->html
 xml->sxml
 node-text
 select-single-node-text
 extract-text extract-string extract-boolean extract-integer extract-real)

(define-type Sxml  (Listof Any))
(define-type SXPath (Sxml -> Sxml))

(require/typed 
 (planet neil/htmlprag:1:7)
 ((html->sxml-2nf html->sxml) (Input-Port -> Sxml)))

(require/typed 
 (planet lizorkin/ssax:2:0/ssax)
 ((ssax:xml->sxml xml->sxml) (Input-Port (Listof (Pair Symbol String)) -> Sxml)))

(require/typed
 (planet lizorkin/sxml:2:1/sxml)
 ((srl:sxml->html sxml->html) ((Listof Any) Output-Port -> Void))
 (sxpath (String (Listof (Pair Symbol String)) -> (Sxml -> Sxml)))
 ((sxml:text node-text) ((Listof Any) -> String)))

;; Returns a function which selects the text from 
;; the nodes selected by the given sxpath.
(define-syntax select-single-node-text
  (syntax-rules ()
    ((_ path-exp ns)
     (let ((sxp (sxpath path-exp ns)))
       (lambda: ((nodelst : (Listof Any)))
	 (node-text (sxp nodelst)))))))

;; Extract the 1st string value (if it exists) as a Real
(: extract-real (Sxml -> (Option Real)))
(define (extract-real sxml)
  (if (pair? sxml)
     (let ((snum (car sxml)))
       (if (string? snum)
	  (let ((rnum (string->number snum)))
	    (if (real? rnum)
	       rnum
	       #f))
	  #f))
     #f))

(: extract-integer (Sxml -> (Option Integer)))
(define (extract-integer sxml)
  (if (pair? sxml)
     (let ((sint (car sxml)))
       (if (string? sint)
	  (let ((inum (string->number sint)))
	    (if (exact-integer? inum)
	       inum
	       #f))
	  #f))
     #f))

(define (extract-string sxml)
  (if (pair? sxml)
     (let ((str (car sxml)))
       (if (string? str)
	  str
	  ""))
     ""))

(: extract-boolean (Sxml -> Boolean))
(define (extract-boolean sxml)
  (if (pair? sxml)
     (let ((flag (car sxml)))
       (if (string? flag)
	  (or (string=? flag "true")
	     (string=? flag "1"))
	  #f))
     #f))

(: extract-text (Sxml -> String))
(define extract-text extract-string)
