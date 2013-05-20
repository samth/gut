;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide:
 [qparam-values (QParam -> (Values String String))]
 [add-qparam (QParam QParams -> QParams)]
 [lowercase-name (QParam -> QParam)]
 [merge-qparams (QParams QParams -> QParams)])

(require
 (only-in "types.rkt"
	  QParams QParam QParam-name QParam-value))

(: qparam-values (QParam -> (Values String String)))
(define (qparam-values qparam)
  (values (QParam-name qparam)
	  (QParam-value qparam)))

(: add-qparam (QParam QParams -> QParams))
(define (add-qparam qparam qparams)
  (cons qparam qparams))

(: merge-qparams (QParams QParams -> QParams))
(define (merge-qparams qps1 qps2)
  (append qps1 qps2))

(: lowercase-name (QParam -> QParam))
(define (lowercase-name qparam)
  (QParam (string-downcase (QParam-name qparam))
	  (QParam-value qparam)))

(: lexical-sort-qparams (QParams -> QParams))
(define (lexical-sort-qparams qparams)
  (: key-lexical (QParam QParam -> Boolean))
  (define (key-lexical p1 p2)
    (let ((p1k (QParam-name p1))
	  (p2k (QParam-name p2)))
      (if (string=? p1k p2k)
	  (string<? (QParam-value p1) (QParam-value p2))
	  (string<? p1k p2k))))
  (sort qparams key-lexical))
