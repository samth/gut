#lang typed/racket/base

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

(require
 (only-in "mimetype-const.rkt"
	  X-WWW-FORM-URLENCODED)
 (only-in "heading.rkt"
	  CONTENT-TYPE)
 (only-in "header.rkt"
          make-header-string get-header get-header-value
          Header Headers))

(: x-www-form-urlencoded? (Headers -> Boolean))
(define (x-www-form-urlencoded? headers)
  (let ((header (get-header CONTENT-TYPE headers)))
    (if header
       (string=? (cdr header) X-WWW-FORM-URLENCODED)
       #f)))


