;;;; package.lisp

(defpackage :cl-oid-connect
  (:use
    #:cl
    #:alexandria
    #:anaphora
    #:clack
    #:cl-json
    #:cljwt
    #:cl-who
    #:drakma
    ;#:lack-middleware-session
    #:ningle
    #:sheeple)
  (:export
    #:redirect-if-necessary
    #:def-route
    #:require-login
    #:oauth2-login-middleware
    #:with-session
    #:session ; private!!
    ))
