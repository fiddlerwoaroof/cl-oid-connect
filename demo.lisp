(ql:quickload :ningle)
(ql:quickload :clack)
(ql:quickload :cl-oid-connect)
(ql:quickload :cl-who)
(ql:quickload :ubiquitous)
(ql:quickload :clack-errors)

(defpackage :cl-oid-connect.demo
  (:use :cl :cl-oid-connect.utils :cl-oid-connect :cl-who))
(in-package :cl-oid-connect.demo)

(defparameter *app* (make-instance 'ningle:<app>))

(def-route ("/login" (params) :app *app*)
  `(200 ()
    ,(with-html-output-to-string (s)
       (:html (:head (:title "Login"))
        (:body
          (:div :class "login-button facebook"
           (:a :href "/login/facebook" "Facebook"))
          (:div :class "login-button google"
           (:a :href "/login/google" "Google")))))))

(def-route ("/" (params) :app *app*)
  (declare (ignore params))
  (ningle:with-context-variables (session)
    (require-login
      (redirect-if-necessary session
        (with-endpoints (gethash :endpoint-schema session)
          `(200 (:content-type "application/json")
            ,(cl-json:encode-json-to-string (gethash :userinfo session))))))))

(cl-oid-connect::setup-oid-connect *app* (userinfo &rest args))

(let ((handler nil))
  (ubiquitous:restore :whitespace)
  (defun stop () (clack:stop (pop handler)))

  (defun start (&optional tmp)
    (cl-oid-connect:initialize-oid-connect
      (ubiquitous:value 'facebook 'secrets)
      (ubiquitous:value 'google 'secrets))
    (let ((server (if (> (length tmp) 1)
                    (intern (string-upcase (elt tmp 1)) 'keyword)
                    :hunchentoot)))
      (push (clack:clackup
              (lack.builder:builder
                :backtrace
                :session
                *app*
                )
              :port 9090
              :server server)
            handler)))

  (defun restart-clack ()
    (do () ((null handler)) (stop))
    (start)))


