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

#lang typed/racket

(provide
 Scheme Path Fragment QParams
 (struct-out QParam)
 (struct-out Url)
 (struct-out Authority))

(define-type Path String)
(define-type Fragment (Option String))
(define-type QParams (Listof QParam))

(define-type Scheme (U 'HTTP 'FTP String))

;; Invariant: name and value are always encoded.
(struct: QParam ([name : String]
		 [value : String]))

(struct: Authority ([user : (Option String)]
                    [host : String]
                    [port : (Option Natural)]))

(struct: Url ([scheme : Scheme]
	      [authority : Authority]
	      [path : Path]
	      [query : QParams]
	      [fragment : Fragment]))
