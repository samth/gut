#lang scribble/base

@begin[(require scribble/manual		
		scribble/base)
       (require (for-label (only-meta-in 0 typed/racket)))]

@title{URIs and URls}
@section{URI}

The original intent of the URI library was for full support of URI genericity, however, the current incarnation is biased for URLs but probably usable for many other flavors of URIs in common use.

@subsection{URI Data Structures}

@defmodule["../../httpclient/uri.rkt"]{

@defstruct*[Uri ([scheme String]
	         [authority  (U False Authority)]
	         [path  String]
	         [query  (Option String)]
	         [fragment  (Option String)]) #:transparent]{
The major compositional structures of a URI.
}

@defstruct*[Authority ([user  (Option String)]
		       [host  String]
		       [port  Integer]) #:transparent]{
The authority structure of a URI.  The host and port are required values.		       
}

@defproc[(make-uri [scheme String] [user (Option String)]		   
		   [host String]   [port Natural]
		   [path String]   [query String]
		   [fragment String]) Uri]{
Construct a uri.		   
}

@defproc[(parse-uri [uristr String]) (Option Uri)]{
Parse the given string into a @racket[Uri].  If the string fails to parse #f is returned.
}
}

@section{URL}

@subsection{URL Encoding / Decoding}

@defmodule["../httpclient/uri/url/encode.rkt"]{

@defproc[(url-encode-string [str String] [space-as-plus Boolean]) String]{
URL encode a string converting reserved characters as percent hex values. 
If @racket[space-as-plus] is true spaces are encoded as @litchar["+"] chars in lieu of encoding as a  @litchar["%20"].
}

@defproc[(url-decode-string [str String] [delim (Option Char)] [decode-plus? Boolean]) String]{
URL decode the given string converting percent hex encoded values.  If @racket[delim] decode until the deliminator char otherwise decode until the entire string.  If @racket[decode-plus?] is true @litchar["+"] chars decode as a @litchar[" "] (a @racket[#\space]).
}

@defproc[(url-decode-from-input-port [str String] [delim (Option Char)] [decode-plus? Boolean]) String]{
Reading chars from the input port URL decoding percent hex encoded values.  If @racket[delim] decode until the deliminator char is found or if not defined until eof-object?.  If @racket[decode-plus?] is true @litchar["+"] chars decode as a @litchar[" "] (a @racket[#\space]).
}
}

@subsection{Query Params}

@defmodule["../httpclient/uri/url/param.rkt"]{

A @racket[Param] is a name value pair intended for use in constructing the URL query string.

@defthing[Param (define-type Param (Pair String String))]{
A Param is typed defined as a pairof strings, the name and value.
}

@defproc[(param [name String] [value String]) Param]{
Constructor for the Param type.		
}

@defproc[(Param? [any Any]) Boolean]{
Type predicate for the Param data type.
}

@defproc[(encode-param [param Param] [space-as-plus Boolean]) Param]{
Construct a new @racket[Param] from the given @racket[Param] percent encoding URL query reserved characters.  If @racket[space-as-plus] is true spaces are encoded as @litchar["+"] chars in lieu of %20.  Both the name and value of the @racket[Param] are encoded.
}

@defthing[Params (define-type Params (Listof Param))]{
A collection of @racket[Param].  @racket[Params] may contain any number of @racket[Param] with the same name, even the same name and value.
}

@defproc[(parse-params [str String]) (Listof Param)]{
Parse out as many name value pairs as are legal from the given string.  Given a partially malformed URL query string several @racket[Param]s may be parsed out with the remainder (unfortunately) silently failing.		       
}

@defproc[(params->query [params Params]) String]{
Generates a URL query string suitable for constructing a URL via @racket[make-uri].  All @racket[Param]'s name and values are encoded.

Names are concatenated with values separated by a @litchar["="] character.  These string snips are then subsequently concatenated together with an interstitial @litchar["&"]. 
}

Example:
@racketblock[
(params->query (list (param "name" "Elmer Fudd") 
                     (param "code" "=Racket=")))
- : String
"name=Elmer%20Fudd&code=%3DRacket%3D"
]
}

@subsection{Path Utilties}

@defmodule["../../httclient/uri/path.rkt"]{

@defproc[(path-split [path String]) (Listof String)]{
Split a URL path into its constituent segments.  For reasons of arcania the first element of a split absolute path is the empty string.

Example:
@racketblock[
(path-split "/a/b/c")
- : (Listof String)
'("" "a" "b" "c")

(path-split "a/b/c")
- : (Listof String)
'("a" "b" "c")
]

}
}
