;;;; package.lisp

(defpackage #:cl-oid-connect.utils
  (:use #:cl #:alexandria #:anaphora #:fwoar.lisputils #:ningle)
  (:export #:vars-to-symbol-macrolets #:with-session-values #:with-endpoints
           #:with-session #:def-route #:gen-state #:valid-state #:my-with-context-variables
           #:string-assoc #:assoc-cdr #:define-auth-entry-point #:define-auth-callback
           #:reject-when-state-invalid #:auth-callback-skeleton #:ensure-logged-in
           #:setup-oid-connect #:check-login #:require-login #:redirect-if-necessary
           #:*oid* #:user-not-logged-in))

(defpackage #:cl-oid-connect.objects
  (:use #:cl #:alexandria #:anaphora #:fwoar.lisputils #:cl-oid-connect.utils #:sheeple)
  (:export #:*fbook-info* #:*goog-info* #:*fbook-endpoint-schema* #:*goog-endpoint-schema*
           #:get-user-info #:get-access-token #:client-id #:secret #:redirect-uri
           #:token-endpoint #:discover-endpoints #:do-auth-request))

(defpackage #:cl-oid-connect
  (:use
    #:cl #:alexandria #:anaphora #:clack #:cl-json #:cljwt #:cl-who #:drakma
    ;#:lack-middleware-session
    #:iterate #:ningle #:lquery #:plump #:sheeple #:fwoar.lisputils
    #:cl-oid-connect.objects #:cl-oid-connect.utils)
  (:export
    #:redirect-if-necessary #:def-route #:require-login #:oauth2-login-middleware #:with-session
    #:assoc-cdr #:session #| private!! |# #:vars-to-symbol-macrolets #:initialize-oid-connect
    #:run-callback-function))

(in-package :cl-oid-connect.objects)

(defvar *fbook-info*)
(defvar *goog-info*)
(defvar *endpoint-schema*)
