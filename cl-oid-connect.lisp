;;;; cl-oid-connect.lisp
;;;; TODO: Need to refactor out server names!!!

#|
 |Copyright (c) 2015 Edward Langley
 |All rights reserved.
 |
 |Redistribution and use in source and binary forms, with or without
 |modification, are permitted provided that the following conditions
 |are met:
 |
 |Redistributions of source code must retain the above copyright notice,
 |this list of conditions and the following disclaimer.
 |
 |Redistributions in binary form must reproduce the above copyright
 |notice, this list of conditions and the following disclaimer in the
 |documentation and/or other materials provided with the distribution.
 |
 |Neither the name of the project's author nor the names of its
 |contributors may be used to endorse or promote products derived from
 |this software without specific prior written permission.
 |
 |THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 |"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 |LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 |FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 |HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 |SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES  INCLUDING, BUT NOT LIMITED
 |TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 |PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 |LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 |NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 |SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 |
 |#

(in-package :cl-oid-connect)
; Should this be here?

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun vars-to-symbol-macrolets (vars obj)
    (iterate:iterate (iterate:for (store key) in (ensure-mapping vars))
                     (iterate:collect `(,store (gethash ,(alexandria:make-keyword key) ,obj))))))

(defmacro with-session-values (vars session &body body)
  (alexandria:once-only (session)
    `(symbol-macrolet ,(vars-to-symbol-macrolets vars session)
       ,@body)))

; This probably should eventually go?
(defmacro with-endpoints (endpoint-schema  &body body)
  `(let* ((*endpoint-schema* ,endpoint-schema))
     ,@body))

(defmacro with-session ((var) &body body)
  `(progn
     (format t "The session var is: ~a it contains: ~a~%"  ,(symbol-name var) ,var)
     (let ((,var (context :session)))
       (format t "The session var is: ~a it now contains: ~a~%"  ,(symbol-name var) ,var)
       ,@body)))

(defmacro def-route ((url args &key (app *oid*) (method :GET)) &body body)
  `(setf (ningle:route ,app ,url :method ,method)
         (lambda ,args
           (declare (ignorable ,@args))
           ,@body)))

(defun gen-state (len)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat len
            do (princ (random 36) stream)))))

(defun valid-state (received-state)
  (let* ((session (context :session))
         (saved-state (gethash :state session)))
    (equal saved-state received-state)))

(defmacro my-with-context-variables ((&rest vars) &body body)
  "This improves fukamachi's version by permitting the variable to be stored somewhere
   besides the symbol corresponding to the keyword."
  `(symbol-macrolet
       ,(loop for (var key) in (ensure-mapping vars)
              for form = `(context ,(intern (string key) :keyword))
              collect `(,var ,form))
     ,@body))

(defparameter *oid* (make-instance 'ningle:<app>))
(setf drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))

(setf =service-info= (object :parents '()
                             :properties '((client-id nil :accessor client-id)
                                           (secret nil :accessor secret))))

(setf =endpoint-schema= (object :parents '()
                                :properties '((auth-endpoint nil :accessor auth-endpoint)
                                              (token-endpoint nil :accessor token-endpoint)
                                              (userinfo-endpoint nil :accessor t)
                                              (auth-scope "openid profile email" :accessor t)
                                              (redirect-uri nil :accessor t))))

(sheeple:defmessage get-user-info (a b))
(sheeple:defmessage get-access-token (a b))

(sheeple:defreply get-user-info ((a =endpoint-schema=) (b sheeple:=string=)))
(sheeple:defreply get-access-token ((a =endpoint-schema=) (b sheeple:=string=)))

(defparameter *endpoint-schema* nil)
(defmacro string-assoc (key alist) `(assoc ,key ,alist :test #'equal))
(defmacro assoc-cdr (key alist &optional (test '#'eql)) `(cdr (assoc ,key ,alist :test ,test)))

(defun discover-endpoints (schema discovery-doc-url &key (gat nil gat-p) (gui nil gui-p))
  "Discover endpoints on the basis of a discovery document stored at a particular url.
   The two keyword arguments define a function to bind to sheeple replies for get-user-token
   and get-access-token."
  (prog1 schema
    (let ((discovery-document (cl-json:decode-json-from-string (drakma:http-request discovery-doc-url))))
      (setf (auth-endpoint schema) (assoc-cdr :authorization--endpoint discovery-document)
            (token-endpoint schema) (assoc-cdr :token--endpoint discovery-document)
            (userinfo-endpoint schema) (assoc-cdr :userinfo--endpoint discovery-document))
      (when gui-p
        (format t "defining gui-p")
        (sheeple:defreply get-user-info ((a schema)) (funcall gui a)))
      (when gat-p
        (format t "defining gat-p")
        (sheeple:defreply get-access-token ((a schema) (b sheeple:=string=))
          (funcall gat a b))))))

(defun do-auth-request (endpoint-schema state)
  (drakma:http-request (auth-endpoint endpoint-schema)
                       :redirect nil
                       :parameters `(("client_id" . ,(client-id endpoint-schema))
                                     ("app_id" . ,(client-id endpoint-schema))
                                     ("response_type" . "code")
                                     ("scope" . ,(auth-scope endpoint-schema))
                                     ("redirect_uri" . ,(redirect-uri endpoint-schema))
                                     ("state" . ,state))))

(defmacro auth-entry-point (name endpoint-schema)
  `(defun ,name (params)
     (declare (ignore params))
     (with-session-values (state endpoint-schema) (context :session)
       (setf state (gen-state 36)
             endpoint-schema ,endpoint-schema)
       (with-endpoints ,endpoint-schema
         (multiple-value-bind (content rcode headers uri) (do-auth-request ,endpoint-schema state)
           (declare (ignore headers))
           (if (< rcode 400) `(302 (:location ,(format nil "~a" uri)))
             content))))))

(defun run-callback-function (endpoint-schema params get-app-user-cb get-login-data)
  (flet ((get-code (params) (assoc-cdr "code" params #'equal)))
    (let ((a-t (get-access-token endpoint-schema (get-code params))))
      (auth-callback-skeleton params (:endpoint-schema endpoint-schema
                                      :auth-session-vars (accesstoken userinfo idtoken app-user))
        (multiple-value-bind (access-token user-info id-token) (funcall get-login-data a-t)
          (setf accesstoken access-token
                userinfo user-info
                idtoken id-token
                app-user (funcall get-app-user-cb user-info id-token access-token)))
        '(302 (:location "/"))))))

(defmacro generate-auth-callback (name endpoint-schema params &body body)
  (with-gensyms (get-app-user-cb cb-params)
    `(defun ,name (,get-app-user-cb)
       (lambda (,cb-params)
         (run-callback-function
           ,endpoint-schema ,cb-params ,get-app-user-cb
           (lambda ,params
             ,@body))))))

(defmacro reject-when-state-invalid (params &body body)
  (alexandria:with-gensyms (received-state)
    (alexandria:once-only (params)
      `(let ((,received-state (cdr (string-assoc "state" ,params))))
         (if (not (valid-state ,received-state))
           '(403 '() "Out, vile imposter!")
        ,@body)))))

(defmacro auth-callback-skeleton (params (&key endpoint-schema auth-session-vars) &body body)
  (alexandria:with-gensyms (session)
    (alexandria:once-only (params endpoint-schema)
      `(reject-when-state-invalid ,params
         (with-endpoints ,endpoint-schema
           (my-with-context-variables ((,session session))
             ,(if (null auth-session-vars)
                `(progn
                   ,@body)
                `(with-session-values ,auth-session-vars ,session
                   ,@body))))))))

(define-condition user-not-logged-in (error) ())

(defmacro ensure-logged-in (&body body)
  "Ensure that the user is logged in: otherwise throw the condition user-not-logged-in"
  (alexandria:with-gensyms (session userinfo)
    `(my-with-context-variables ((,session session))
       (with-session-values ((,userinfo userinfo)) ,session
         (if (null ,userinfo)
           (error 'user-not-logged-in)
           (progn ,@body))))))

(flet ((handle-no-user (main-body handler-body)
         `(handler-case (ensure-logged-in ,@main-body)
            (user-not-logged-in (e) (declare (ignorable e))
                                ,@handler-body))))

  (defmacro check-login (&body body)
    "Returns an HTTP 401 Error if not logged in."
    (handle-no-user body `('(401 () "Unauthorized"))))

  (defmacro require-login (&body body)
    "Redirects to /login if not logged in."
    (handle-no-user body
                    `((with-session-values (next-page) (context :session)
                        (setf next-page (lack.request:request-path-info *request*))
                        '(302 (:location "/login")))))))

(defparameter *fbook-info* (sheeple:clone =service-info=))
(defparameter *goog-info* (sheeple:clone =service-info=))
(defparameter *goog-endpoint-schema* (defobject (=endpoint-schema= *goog-info*)))

(defproto *fbook-endpoint-schema* (=endpoint-schema= *fbook-info*)
          ((auth-endpoint "https://www.facebook.com/dialog/oauth")
           (token-endpoint "https://graph.facebook.com/v2.3/oauth/access_token")
           (userinfo-endpoint "https://graph.facebook.com/v2.3/me")
           (auth-scope "email")
           (redirect-uri  "http://srv2.elangley.org:9090/oidc_callback/facebook")))

(sheeple:defreply get-access-token ((endpoint-schema *fbook-endpoint-schema*) (code sheeple:=string=))
  (cl-json:decode-json-from-string
    (drakma:http-request (token-endpoint endpoint-schema)
                         :method :post
                         :redirect nil
                         :parameters `(("code" . ,code)
                                       ("client_id" . ,(client-id endpoint-schema))
                                       ("app_id" . ,(client-id endpoint-schema))
                                       ("client_secret" . ,(secret endpoint-schema))
                                       ("redirect_uri" . ,(redirect-uri endpoint-schema))
                                       ("grant_type" . "authorization_code")
                                       ("")
                                       ))))

(sheeple:defreply get-user-info ((endpoint-schema *fbook-endpoint-schema*) (access-token sheeple:=string=))
  (let ((endpoint (userinfo-endpoint endpoint-schema)))
    (cl-json:decode-json-from-string
      (drakma:http-request endpoint
                           :parameters `(("access_token" . ,access-token))))))

(defun load-provider-secrets (provider-info secrets)
  (setf (client-id provider-info) (assoc-cdr :client-id secrets)
        (secret provider-info) (assoc-cdr :secret secrets)))

(defun goog-get-access-token (endpoint-schema code)
  (cl-json:decode-json-from-string
    (drakma:http-request (token-endpoint endpoint-schema)
                         :method :post
                         :redirect nil
                         :parameters `(("code" . ,code)
                                       ("client_id" . ,(client-id endpoint-schema))
                                       ("client_secret" . ,(secret endpoint-schema))
                                       ("redirect_uri" . ,(redirect-uri endpoint-schema))
                                       ("grant_type" . "authorization_code")))))

(defun load-goog-endpoint-schema ()
  (discover-endpoints *goog-endpoint-schema*
                      "https://accounts.google.com/.well-known/openid-configuration"
                      :gat #'goog-get-access-token)
  (setf (redirect-uri *goog-endpoint-schema*) "http://srv2.elangley.org:9090/oidc_callback/google"))

(sheeple:defreply get-user-info ((endpoint-schema *goog-endpoint-schema*) (access-token sheeple:=string=))
  (format t "getting user data: ~a~%" "blarg")
  (let ((endpoint (userinfo-endpoint endpoint-schema)))
    (cl-json:decode-json-from-string
      (drakma:http-request endpoint
                           :parameters `(("alt" . "json")
                                         ("access_token" . ,access-token))))))

(auth-entry-point google-login-entry *goog-endpoint-schema*)
(auth-entry-point facebook-login-entry *fbook-endpoint-schema*)

(generate-auth-callback google-callback *goog-endpoint-schema* (a-t)
  (labels ((get-real-access-token (a-t) (assoc-cdr :access--token a-t))
           (get-id-token (a-t) (cljwt:decode (assoc-cdr :id--token a-t) :fail-if-unsupported nil)))
    (let ((access-token (get-real-access-token a-t)))
      (values access-token
        (get-user-info *goog-endpoint-schema* access-token)
        (get-id-token a-t)))))

(generate-auth-callback facebook-callback *fbook-endpoint-schema* (a-t)
  (labels ((get-id-token (a-t) (assoc-cdr :access--token a-t)))
    ; ^-- access--token is not a mistake here
    (let ((id-token (get-id-token a-t)))
      (values a-t (get-user-info *fbook-endpoint-schema* id-token) id-token))))

(defun initialize-oid-connect (facebook-info google-info)
  "Load the Google and Facebook app secrets and initialize Google's openid-configuration
   form its well-known document"
  (load-provider-secrets *fbook-info* facebook-info)
  (load-provider-secrets *goog-info* google-info) 
  (load-goog-endpoint-schema))

(defun bind-oid-connect-routes (app &optional (login-callback #'identity))
  (setf (route app "/login/google" :method :get) (lambda (params) (google-login-entry params))
        (route app "/login/facebook" :method :get) (lambda (params) (facebook-login-entry params))
        (route app "/oidc_callback/google" :method :get) (google-callback login-callback)
        (route app "/oidc_callback/facebook" :method :get) (facebook-callback login-callback)))

(defmacro setup-oid-connect (app args &body callback)
  `(bind-oid-connect-routes ,app (lambda ,args ,@callback)))

(defmacro redirect-if-necessary (sessionvar &body body)
  (with-gensyms (session)
    `(let* ((,session ,sessionvar)
            (next-page (gethash :next-page ,session)))
       (if (and (not (null next-page))
                (not (string= next-page (lack.request:request-path-info *request*))))
         (progn
           (setf (gethash :next-page ,session) nil)
           `(302 (:location ,next-page)))
         ,@body))))
