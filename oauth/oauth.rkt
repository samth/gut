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

(provide
 (struct-out OAuth))

(provide:
 [oauth-authorization-header (OAuth Method Url -> Param)])

(require
 racket/pretty
 (only-in type/text
	  weave-string-separator)
 (only-in type/opt
	  opt-map-orelse-value
	  opt-map-orelse)
 (only-in crypto/hmac
	  hmac-sha1)
 (only-in crypto/base64
	  base64-encode)
 (only-in net/uri/url/url
	  scheme->string
	  Authority-host
	  QParam QParams QParam-name QParam-value
	  Uri Url Uri-scheme Url-authority Url-path Url-query Url-fragment)
 (only-in httpclient/param
	  Param Params  param->noencode-string
	  param make-params empty-params
	  param-keyval params->query add-param)
 (only-in httpclient/http11
	  Method http-method->string)
 (only-in "encode.rkt"
	  encode))

(define oauth-version-key	   "oauth_version")
(define oauth-nonce-key		   "oauth_nonce")
(define oauth-timestamp-key	   "oauth_timestamp")
(define oauth-consumer-key-key	   "oauth_consumer_key")
(define oauth-signature-method-key "oauth_signature_method")
(define oauth-signature-key	   "oauth_signature")

(define oauth-version-val "1.0")

(define-type SignatureMethod (U 'HMAC-SHA1 'PLAINTEXT 'RSA-SHA1))

(struct: OAuth ([consumer-key : String]
		[consumer-secret : String]
		[signature-method : SignatureMethod]))

(: generate-nonce (-> String))
(define (generate-nonce)
  (weave-string-separator "" (for/list ([i (in-range 10)])
				    (number->string (random 10)))))

(: current-timestamp (-> String))
(define (current-timestamp)
  (number->string (current-seconds)))

(: signature-method->string (SignatureMethod -> String))
(define (signature-method->string method)
  (symbol->string method))

(: canonicalize-param-order (Params -> Params))
(define (canonicalize-param-order params)
  (: key-lexical (Param Param -> Boolean))
  (define (key-lexical p1 p2)
    (let-values (((p1k  p1v) (param-keyval p1))
		 ((p2k  p2v) (param-keyval p2)))
      (if (string=? p1k p2k)
	  (string<? p1v p2v)
	  (string<? p1k p2k))))
  (sort params key-lexical))

;; Potentially the consumer key may require encoding.
(: build-base-encoded-params (OAuth String String -> Params))
(define (build-base-encoded-params oauth nonce ts)
  (let ((sig-method (signature-method->string (OAuth-signature-method oauth))))
    (make-params
     (param oauth-consumer-key-key (encode (OAuth-consumer-key oauth)))
     (param oauth-signature-method-key sig-method)
     (param oauth-timestamp-key ts)
     (param oauth-nonce-key nonce)
     (param oauth-version-key oauth-version-val))))

(: quote-params-value (Params -> Params))
(define (quote-params-value params)
  (map (λ: ((p : Param))
	   (let-values (((k v) (param-keyval p)))
	     (param k (format "~s" v))))
       params))

(: params->encoded-params (Params -> Params))
(define (params->encoded-params ps)
  (map (λ: ((p : Param))
	   (let-values (((k v) (param-keyval p)))
	     (param (encode k) (encode v))))
       ps))

(: qparam->encoded-parm (QParams -> Params))
(define (qparam->encoded-parm qps)
  (map (λ: ((qp : QParam))
	   (param (encode (QParam-name qp)) (encode (QParam-value qp))))
       qps))

(: build-oauth-signing-params (Params Url -> Params))
(define (build-oauth-signing-params base-params url)
  (let ((url-query-params (Url-query url)))
    (canonicalize-param-order (append (qparam->encoded-parm url-query-params)
				      base-params))))

(: build-signee (Method Url Params -> String))
(define (build-signee method url oauth-params)
  (let ((method (http-method->string method))
	(path (encode (string-append (scheme->string (Uri-scheme url))
				     "://"
				     (opt-map-orelse-value (Url-authority url)
						      Authority-host "")
				     (Url-path url))))
	(qparams (encode (params->query oauth-params))))
    (weave-string-separator "&" (list method path qparams))))

(: signor (String String -> String))
(define (signor signee key)
  (base64-encode (hmac-sha1 (string-append key "&") signee)))

(: build-oauth-signature-params (OAuth Method Url -> Params))
(define (build-oauth-signature-params oauth method url)
  (let* ((nonce (generate-nonce))
	 (ts    (current-timestamp))
	 (base-params (build-base-encoded-params oauth nonce ts))
	 (signee-params (build-oauth-signing-params base-params url))
	 (signee (build-signee method url signee-params))
	 (sig-param (param oauth-signature-key (signor signee (OAuth-consumer-secret oauth)))))
    (quote-params-value (add-param sig-param base-params))))

(: oauth-authorization-header (OAuth Method Url -> Param))
(define (oauth-authorization-header oauth method url)
  (let* ((auth-param-strs (map param->noencode-string 
			       (build-oauth-signature-params oauth method url)))
	 (auth-str (weave-string-separator "," auth-param-strs)))
    (param "Authorization" (string-append "OAuth " auth-str))))
