#lang scribble/doc

@begin[(require scribble/manual		
		scribble/base)
       (require (for-label (only-meta-in 0 typed/racket)))]

@title{HTTP Client}

@defmodule["../../httpclient/http/http11.rkt"]

@section{HTTP Data Structures}

Performs synchronous HTTP 1.1 invocations against a server.  Full HTTP 1.1 support is not yet in place.  The current incarnation supports gzip compression, SSL and chunk encoding.

@defstruct*[RequestLine ([method   Method]
                         [path     String]
                         [version  Version]) #:transparent]{
                                                            The HTTP standard header line for a request.
                                                            }
                                                           
@defstruct*[RequestHeader ([request RequestLine]
		      	   [headers (Listof Header)]) #:transparent]{
The HTTP protocol header for a requests consisting of the request line, e.g., @racket{GET /index.html HTTP/1.1} and a set of HTTP headers (name value pairs).
}

@defstruct*[StatusLine ([version Version]
	       	        [code    Integer]
		        [msg     String]) #:transparent]{
The HTTP status line returned by a server responding to a request.
}

@defstruct*[ResponseHeader ([status  StatusLine]
	  		    [headers  (Listof Header)]) #:transparent]{
The full HTTP response header from a server responding to a request. Consists of the status line
and accompaning HTTP headers (name value pairs).
}

@defstruct*[HTTPConnection ([header ResponseHeader]
			    [out Output-Port]
			    [in  Input-Port]
			    [real-in (Option Input-Port)]) #:transparent]{
An HTTP connection.  @racket{header} HTTP server's response status and headers.
@racket{out} is the output port which the client uses to write the request and any accompaning payload to the server.
@racket{in} is the overall @italic{effective} input port to be used by the user application.
@racket{real-in} is the true low-level socket input port.  
The @racket{in} port may be connected by one or more @racket{pipe}s which, if present, perform one or more transforms on the data read from the raw socket port. 

For example, if the HTTP request has @italic{Accept-Encoding: gzip} in the request header the server response payload at the @racket{real-in} socket port is gzip compressed.  The client application reading reading from @racket{in} port receives a gzip uncommpressed stream of data as this library automatically inserts unzipping pipe between the @racket{real-in} port and the client used @racket{HTTPConnection} @racket{in} port. 

Similarly for HTTP chunked encoding. This library automatically inserts a @racket{pipe} between the @racket{HTTPConnection} @racket{real-in} port and the client application reads from the "de-chunked" @racket{HTTPConnection} @racket{in} port.
}

@defstruct*[HTTPPayload ([mime    String]
	   	         [md5     (Option String)]
		         [length  (Option Index)]
		         [inport  Input-Port]) #:transparent]{
Certain HTTP methods such as PUT and POST requests send payload data from the client.  This data structure captures encapsulates a payload as an @racket{Input-Port}.  If the optional MD5 hash for the payload is provided the hash value is sent to the server as part of the HTTP request.

If the @racket{length} is provided the HTTP request sends this value as a @italic{Content-Length} HTTP header.  If the @italic{Content-Length} is not provided the HTTPClient library uses @italic{Transfer-Encoding: chunked} in the request.

Chunked encoding allows a client to send a payload of unknown size to the server (or from the server to the client) by buffering a streamed payload as @italic{chunks} of data.

The mime type string specifies the format of the request payload, e.g., @italic{application/json} or @italic{application/atom+xml}.
}

@section{Invoking An HTTP Request}

The various HTTP actions or methods are defined as a data type.

@defthing[Method (U 'GET 'PUT 'POST 'DELETE 'HEAD 'CONNECT 'OPTIONS 'TRACE)]{
A type enumeration of valid HTTP methods.
}

@defproc[(http-invoke [method Method] 
		      [url Uri] 
		      [headers Headers]
		      [payload (Option Payload)]) HTTPConnection]{
Invoke an HTTP request. If a @racket{Payload} is provided it is sent to the server as part of the invocation with the appropriate headers implicitly added.  The server response if available via the returned @racket{HTTPConnection}.
}

There are several utility functions to quickly check the server's response.  The full server response is available in @racket{ResponseHeader} contained in the @racket{HTTPConnection}.

@defproc[(http-successful? [connection HTTPConnection]) Boolean]{
Utility method that returns @racket{#t} if a successful (2XX) HTTP code was returned by the server.
}

@defproc[(http-status-code [connection HTTPConnection]) Integer]{
Returns the HTTP status code value returned by the server.			       
}

@defproc[(http-has-content? [connection HTTPConnection]) Boolean]{
Did the server return any content (a payload) as part of the response.  If @racket{#t} this payload is available via the @racket{HTTPConnection-in} accessor procedure.
}

@defproc[(http-close-connection [connection HTTPConnection]) Void]{
Properly close the connection to the server.  Currently a close @italic{must} be explicitly performed regardless as to whether the server sent a response payload as part of the response.
}

@section{Headers}

Headers are name value pairs defined in the HTTP 1.1 specification.

@defmodule["../httpclient/http/header.rkt"]{

@defthing[Header (define-type Header (Pair String String))]{
A Header is typed defined as a pairof strings, the name and value.
}

@defthing[Headers (define-type Headers (Listof Header))]{
A list of Header.  No effort is made enforce duplicate @racket{Header} elements.
}

@defproc[(Header? [any Any]) Boolean]{
Type predicate for the Header data type.
}

@defproc[(make-header [name String] [value String]) Header]{
Constructor for the Header type.		
}

@defproc[(add-header [name String] [value String] [headers Headers]) Headers]{
Utility method to add a @racket{Header}.		     
}

@defproc[(get-header [name String] [headers Headers]) (Option Header)]{
Find and return the first @racket{Header} with the given @racket{name}.
}

@defproc[(get-header-value [name String] [headers Header]) (Option String)]{
Similar to @racket{get-header} except the found @racket{Header}'s value is returned.
}

@deftogether[[
@defproc[(host-header [value String]) Header]
@defproc[(agent-header [value String]) Header]
@defproc[(date-header  [value String]) Header]
@defproc[(content-type [value String]) Header]
@defproc[(content-length [value String]) Header]
@defproc[(content-md5    [value String]) Header]
]]{
Utility procedures that construct common HTTP header values.  Generally are not required as this library generally implicitly inserts these headers.  @italic{Deprecated and will be removed.}
}

}
