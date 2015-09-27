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
    #:lass
    #:lquery
    #:plump
    #:sheeple
    #:whitespace.utils
    )
  (:export
    #:redirect-if-necessary
    #:def-route
    #:require-login
    #:oauth2-login-middleware
    #:with-session
    #:assoc-cdr
    #:session ; private!!
    #:vars-to-symbol-macrolets
    ))

