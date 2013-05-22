#| Parse routines for RSS |#

#lang typed/racket/base

(provide
 fetch-rss
 sx-items sx-title sx-description sx-link sx-pubdate sx-content:encoded
 sx-media-content sx-media-content-url sx-media-content-medium
 sx-media-content-width sx-media-content-height)

(require
 racket/pretty
 (only-in net/uri/url/url
	  Url-authority Authority-host
	  Url url->string)
 (only-in net/http/http11
	  http-invoke http-close-connection
	  HTTPConnection-in)
 (only-in net/http/header
	  make-header
	  agent-header
	  host-header)
 (only-in format/xml/sxml
	  Sxml SXPath
	  sxpath xml->sxml select-single-node-text))

(: content-ns (Pair Symbol String))
(define content-ns
  '(content . "http://purl.org/rss/1.0/modules/content/"))

(: media-ns (Pair Symbol String))
(define media-ns
  '(media . "http://search.yahoo.com/mrss"))

(: base-nss (Listof (Pair Symbol String)))
(define base-nss
  (list media-ns))

(: sx-items ((Listof Any) -> (Listof Any)))
(define sx-items
  (sxpath "/rss/channel/item" '()))

(: sx-title ((Listof Any) -> String))
(define sx-title
  (select-single-node-text "/title" '()))

(: sx-description ((Listof Any) -> String))
(define sx-description
  (select-single-node-text "/description" '()))

(: sx-link ((Listof Any) -> String))
(define sx-link
  (select-single-node-text "/link" '()))

(: sx-pubdate ((Listof Any) -> String))
(define sx-pubdate
  (select-single-node-text "/pubDate" '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following are RSS 1.0 module extension used in RSS2.0 by BLIPPR
;; Who are these people???
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: sx-content:encoded ((Listof Any) -> String))
(define sx-content:encoded
  (select-single-node-text "content:encoded" (list content-ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following media elements are Yahoo RSS2.0 extentions
;; See  http://search.yahoo.com/mrss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; relative from an item
;; list of media:content elements
(: sx-media-content ((Listof Any) -> (Listof Any)))
(define sx-media-content
  (sxpath "/media:group/media:content" base-nss))

(: sx-media-content-url ((Listof Any) -> String))
(define sx-media-content-url
  (select-single-node-text "/media:url" base-nss))

(: sx-media-content-medium ((Listof Any) -> String))
(define sx-media-content-medium
  (select-single-node-text "/media:medium" base-nss))

(: sx-media-content-height ((Listof Any) -> String))
(define sx-media-content-height
  (select-single-node-text "/media:height" base-nss))

(: sx-media-content-width ((Listof Any) -> String))
(define sx-media-content-width
  (select-single-node-text "/media:width" base-nss))

;; fetch RSS2.0 content and parse to SXML
;; on parse error returns a '()
(: fetch-rss (Url -> (Listof Any)))
(define (fetch-rss uri)
  (let ((headers  `(,(agent-header "curl/7.16.4 (x86_64-redhat-linux-gnu) libcurl/7.16.4 OpenSSL/0.9.8b zlib/1.2.3 libidn/0.6.8"))))
    (let ((connection (http-invoke 'GET uri headers #f)))
      (with-handlers [(exn:fail? (lambda (ex)
				   ((error-display-handler) "ERROR in S3 invocation." ex)
				   (displayln ex)
				   (http-close-connection connection)
				   (raise ex #t)))]
		     (let ((results (xml->sxml (HTTPConnection-in connection) '())))
		       (http-close-connection connection)
		       results)))))
