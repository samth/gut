#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/basic)	
       (require (for-label (only-meta-in 0 typed/racket)))]
       
@title[#:tag "top"]{@bold{AWS}: SQS - Amazon WebServices}
@declare-exporting[aws]

by Ray Racine (@tt{ray dot racine at gmail dot com})

This library provides various Web and Internet related functionality.

@table-of-contents[]

@section{UUID - Version 1 GUIDs per RFC4122}

@subsection{OS Support}

The functionality for version 1 UUIDs uses the system MAC address.  Current logic assumes a Linux system wherein the MAC address are found in
"/sys/class/net/eth0/address", "/sys/class/net/wlan0/address", etc.



@defmodule["../uuid.rkt"]{

Currently SQS configuration is hardcoded in a "config.rkt" file and reviewed and modified to suit.

@defthing[sqs-api-version String]{
The version of the SQS API to use.  This should be changed as Amazon releases new versions of the SQS API.			  
}

@defthing[sqs-host String]{
The SQS host to which all SQS commands are sent.  This should be changed to match your desired AWS facility.  Ideally this should be say a parameterizable configuration, but for now is a hard code.
}

@defthing[sqs-auth-version String]{
The version of authentication that SQS is using.  For current SQS  api, "2011-10-01" version "2" authentication is used.  This value is sent as part of the request and should in general not be changed.
}

@defthing[signature-method String]{
The signing method used to create the authentication header.  This value is also sent as part of the request.  For the current SQS api, "2011-10-01" HMAC-SHA256 is used and should in general not be changed.
}

@defthing[sqs-ns String]{
The namespace used in the XML contained in payload of an SQS HTTP response.
}

}

@subsection{AWS Authentication}

AWS uses a set of credentials which include a private key or a private session key to perform an signature of the request and then sending the signature as part of the request.

Currently this library expects and requires that a valid set of credentials is loadable from "~/.awscreds.sexp".

@subsubsection{AWS Credentials}


@defproc[(current-aws-credential) (Parameterof AwsCredential)]{
A parameterization containing the credentials used by this library authorizing an AWS call.  An initial set of credential information is loaded during module initialization from a given path.  Module initialization will fail if the expected credential file is not found.
}

@defthing[default-cred-path String]{
Currently defined to be the path "~/.awscreds.sexp".  It is an error if this file does not exist.
}

At a minimum this file must contain the AWS account access key and secret key in the following sexp alist form.

@racketblock[
((access-key . "ACCESSKEY")
 (secret-key . "secretkey"))
]

@defstruct*[BaseCredential ([access-key String] [secret-key String]) #:transparent]{
An abstract base structure which contains the minimal set of credential information an AWS account access and secret key.
}

@defstruct*[(SessionCredential BaseCredential) ([token String]
			       		        [expiration Time]) #:transparent]{
Contains an AWS STS (Security Token Service) issued token and its associated expiration time stamp.  Some AWS services such as DynamoDB require session authorization.  A valid session is token is implicitly maintained by this AWS library.
}

@defstruct*[(AwsCredential BaseCredential)  ([account-id String]
			                     [associatate-tag String]
                                             [session (Option SessionCredential)]) #:mutable #:transparent]{
Extends @racket{BaseCredential} and contains the full set of AWS credentialing information.  The @racket{account-id} and @racket{associate-tag} are used for AWS affiliate program API which are not contained in this planet library.  If these values are not found in the home directory ".awscreds.sexp" file their values are an empty string.

The session credential field holds the current (if any) ephemeral session credentials from AWS STS security token service.

The library will automatically maintain a valid session credential from STS by implicitly obtaining one if it is required or obtaining a fresh token if the current token has expired.
}

@defproc[(load-credential [path Path]) AwsCredential]{
Explicitly read in an sexp alist property file containing AWS credential information.  The library implicitly loads "~/.awscreds.sexp" in module initialization, therefore use of this procedure is unnecessary for normal usage.
}

@defproc[(set-aws-credential! [credential AwsCredential]) Void]{
Utility procedure to set the @racket{current-aws-credential} parameter.  Not required for normal use as the library loads and initializes the @racket{current-aws-credential} parameter implicitly from "~/.awscreds.sexp" on initialization.			       
}


@subsection{STS}
@defmodule["../aws/sts/stc.rkt"]

@defproc[(get-session-token [duration-secs Exact-Nonnegative-Integer]) SessionCredential]{
Obtains a session token valid for the given duration in seconds from the AWS STS service.
}

@subsection{S3 API}

@subsection{S3 URI}

@defproc[(s3-uri-path->prefix [path String]) String]{
S3 API prefixes are relative path strings to the bucket.  A URI path is absolute.
Convert a S3 Uri, s3://mybucket/a/b/c, path, "/a/b/c" to a S3 prefix "a/b/c".
}

@subsection{S3 Bucket Types}

@defstruct*[Buckets ([owner Owner] [buckets (Listof Bucket)]) #:transparent]{
Structure return by the @racket{list-buckets} call to S3.		    
}

@defstruct*[Owner ([id String] [creation-date String]) #:transparent]{
Ownership information associated with an S3 bucket.		  
}

@defstruct*[Bucket ([name String] [creation-date String]) #:transparent]{
Bucket meta information returned by S3 in a @racket{list-buckets} call.		   
}

@defstruct*[S3Response ([http Result]
		        [sxml Sxml]) #:transparent]{
Contains the low level HTTP response header, e.g. the HTTP code and message, and the S3 XML response parsed into sxml.  Eventually all S3 API calls will be fully parsed into appropriate response structures.
}
@subsubsection{S3 Bucket API}

@hyperlink["http://docs.amazonwebservices.com/AmazonS3/latest/API/APIRest.html"]{
See AWS S3 documentation for detailed information on the various parameters.
}


@defproc[(list-buckets) Buckets]{
Obtains a list of owned S3 buckets.			
}

@defproc[(create-bucket [bucket-name String]) S3Response]{
Create a bucket on S3 with the given name.
}

@defproc[(delete-bucket [bucket-name String]) S3Response]{
Deletes the given S3 bucket 
}

@subsubsection{S3 Object Types}

@defstruct*[Object ([key String] 
		    [last-modified String]
		    [etag String]
		    [size Integer]
		    [owner Owner]) #:transparent]{
The meta information associated with an S3 object.
}

@defstruct*[Objects ([name  String]
		     [prefix String]
		     [marker String]
		     [max-keys Integer]
		     [is-truncated Boolean]
		     [objects (Listof Object)]) #:transparent]{
Result information returned from a call to @racket{list-bucket-objects}.		     
} 

@subsubsection{S3 Object API}

@hyperlink["http://docs.amazonwebservices.com/AmazonS3/latest/API/APIRest.html"]{
See AWS S3 documentation for detailed information on the various parameters.
}

@defproc[(list-bucket-objects [bucket String] [prefix String] 
			      [delimiter String] [marker String] [max Integer]) Objects]{
Lists the objects contained in the given bucket.
}

@defproc[(put-object [bytes Bytes] [bucket String] [path String]) S3Response]{
Stores a blob of bytes into a bucket under a path name.		     
}

@defproc[(put-file-object [path-to-file String] [bucket String] [path String]) S3Response]{
Stores a local on disk file up on S3 in a provided path.
}

@defproc[(delete-object [bucket String] [path String]) S3Response]{
Deletes an S3 stored object.
}

@subsection{SQS}

@defmodule[aws/sqs/sqs]

@defproc[(list-queues [prefix (Option String)]) (U SQSError SQSListQueuesResp)]{
List all SQS queues.  If the optional prefix is provided only those queues starting
with the prefix are listed.  e.g. "/txs" would include "/txs/q1" and not "/orders/qs".}

@racketblock[
(: send-message (String String -> (U SQSError Void)))
(define (send-message queue-path msg)
  (sqs-invoke queue-path 'SendMessage (send-message-request msg)))
]

@defproc[(send-message [queue-path String] [msg String]) (U SQSError SQSSendResp)]{
Sends a message to the given gueue. The queue-path should be absolute, e.g. "/myqueue".}


@defthing[AttributeName (U 'All 'SenderId 'SentTimestamp 
                           'ApproximateReceiveCount 
			        'ApproximateFirstReceiveTimestamp)]{
A define-type of an enumeration of allowable code values which specify the attribute values to be returned by SQS when receiving a message.			
}

@defproc[(receive-message [queue-path String] 
			  [attributes (Listof AttributeName)]
			  [visibility-timeout Exact-Nonnegative-Integer]
			  [max-msgs Exact-Nonnegative-Integer]) (U SQSError SQSReceiveResp)]{
Receives a message from the given queue.
@hyperlink["http://docs.amazonwebservices.com/AWSSimpleQueueService/latest/APIReference/Query_QueryReceiveMessage.html"]{See SQS doc recieve API doc for parameter semantics.}
}

@subsection{DynamoDB}

@subsubsection{Table Types}

@defstruct*[CreateTableResp ([name String]
			     [status TableStatus]
			     [create Float]
			     [capacity Throughput]
			     [schema KeySchema]) #:transparent]{
Response information from a @racket{create-table} call.			     
}

@defstruct*[Throughput ([read  Natural]
		        [write Natural]) #:transparent]{
The read and write throughput of a table.			
}

@defthing[TableStatus (U 'Active 'Deleting 'Creating)]{
The status of a table.
}

@defstruct*[ListTablesResp ([names (Listof String)] [last STring])
#:transparent]{ Information returned from a @racket{list-tables} call.  The
@racket{last} value can be used to initiate a second @racket{list-tables} call
to start listing from there.}

@subsubsection{Table API}

@defproc[(create-table [name String]
		       [hash-key Key]
		       [range-key (Option Key)]
		       [throughput Throughput]) (U DDBFailure CreateTableResp)]{
Creates a DynamboDB table.	       
}

@defproc[(delete-table [name String]) (U DDBFailure DeleteTableRep)]{
Deletes the table.
}

@defproc[(describe-table [name String]
			 [schema (Option KeySchema)]
			 [size Integer]
			 [creation Float]
			 [status TableStatus]
			 [capacity Throughput]) (U DDBFailure DescribeTableResp)]{
Meta data information for a table.
}

@defproc[(list-tables [start-from (Option String)]
		      [count Natural]) ListTablesResp]{
List tables.  If @racket{start-from} is given the listing starts lexically after the string.  Count is maximum number of tables to return in the response.
}

