#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/basic)	
       (require (for-label (only-meta-in 0 typed/racket)))]
       
@table-of-contents[]

@section{UUID - Version 1 GUIDs per RFC4122}

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