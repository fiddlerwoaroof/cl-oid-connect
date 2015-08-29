;;;; cl-oid-connect.lisp
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
(setf drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))

(sheeple:defproto =service-info= ()
                  ((client-id nil :accessor t)
                   (secret nil :accessor t)))

(defvar *FBOOK-INFO* (sheeple:clone =service-info=))
(defun load-facebook-info (loadfrom)
  (setf *FBOOK-INFO* 
        (with-open-file (fbook-info (truename loadfrom))
          (let* ((data (yason:parse fbook-info))
                 (client-id (gethash "client-id" data))
                 (secret (gethash "secret" data)))
            (sheeple:defobject (=service-info=)
                               ((client-id client-id)
                                (secret secret)))))))

(defvar *GOOG-INFO* (sheeple:clone =service-info=))
(defun load-google-info (loadfrom)
  (setf *GOOG-INFO*
        (with-open-file (goog-info (truename loadfrom))
          (let* ((data (yason:parse goog-info))
                 (client-id (gethash "client-id" data))
                 (secret (gethash "secret" data)))
            (sheeple:defobject (=service-info=)
                               ((client-id client-id)
                                (secret secret)))))))

(load-facebook-info #p"facebook-secrets.json")
(load-google-info #p"google-secrets.json")

(defmacro string-assoc (key alist) `(assoc ,key ,alist :test #'equal))
(defmacro assoc-cdr (key alist) `(cdr (assoc ,key ,alist)))

(sheeple:defproto =endpoint-schema= ()
                  ((auth-endpoint nil :accessor t)
                   (token-endpoint nil :accessor t)
                   (redirect-uri nil :accessor t)))
(sheeple:defmessage get-user-info (a b))
(sheeple:defmessage get-access-token (a b))

(sheeple:defreply get-user-info ((a =endpoint-schema=) (b sheeple:=string=)))
(sheeple:defreply get-access-token ((a =endpoint-schema=) (b sheeple:=string=)))

(defun discover-endpoints (service-info discovery-doc-url &key (gat nil gat-p) (gui nil gui-p))
  (let ((discovery-document (cl-json:decode-json-from-string (drakma:http-request discovery-doc-url)))
        (schema (sheeple:object :parents `(,=endpoint-schema= ,service-info))))

    (setf (auth-endpoint schema) (assoc-cdr :authorization--endpoint discovery-document))
    (setf (token-endpoint schema) (assoc-cdr :token--endpoint discovery-document))

    (if gui-p (sheeple:defreply get-user-info ((a schema)) (funcall gui a)))
    (if gat-p (sheeple:defreply get-access-token ((a schema) (b sheeple:=string=))
                (funcall gat a b)))

    schema))

; This probably should eventually go?
(defvar *endpoint-schema* nil)
(defmacro with-endpoints (endpoint-schema  &body body)
  `(let* ((*endpoint-schema* ,endpoint-schema))
     ,@body))

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

; goog is well behaved
(defvar *goog-endpoint-schema*
  (discover-endpoints *GOOG-INFO* "https://accounts.google.com/.well-known/openid-configuration"
                      :gat #'goog-get-access-token))
(setf (redirect-uri *goog-endpoint-schema*)   "http://srv2.elangley.org:9090/oidc_callback/google")


; fbook needs personal attention
(defproto *fbook-endpoint-schema* (=endpoint-schema= *fbook-info*)
          ((auth-endpoint "https://www.facebook.com/dialog/oauth")
           (token-endpoint "https://graph.facebook.com/v2.3/oauth/access_token")
           (userinfo-endpoint "https://graph.facebook.com/v2.3/me" :accessor t)
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

(defvar *fbook-mw*
  (lambda (app)
    (lambda (env)
      (with-fbook-endpoints
        (format t "~a" *client-id*)
        (funcall app env)))))

(defvar *goog-mw*
  (lambda (app)
    (lambda (env)
      (with-goog-endpoints
        (funcall app env)))))

(defun do-auth-request (endpoint-schema state)
  (format t "~%client-id: ~a~%" (auth-endpoint endpoint-schema))
  (drakma:http-request (auth-endpoint endpoint-schema)
                       :redirect nil
                       :parameters `(("client_id" . ,(client-id endpoint-schema))
                                     ("app_id" . ,(client-id endpoint-schema))
                                     ("response_type" . "code")
                                     ("scope" . "email")
                                     ("redirect_uri" . ,(redirect-uri endpoint-schema))
                                     ("state" . ,state))))

(defun gen-state (len)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat len
            do (princ (random 36) stream)))))

(defvar *app* (make-instance 'ningle:<app>))
(defmacro def-route (url args &body body)
  `(setf (ningle:route *app* ,url)
         #'(lambda ,args
             (declare (ignorable ,@args))
             ,@body)))

(defmacro check-state (received-state then else)
  (alexandria:with-gensyms (saved-state session)
    `(let* ((,session (context :session))
            (,saved-state (gethash :state ,session)))
       (if (equal ,saved-state ,received-state)
         ,then
         ,else))))

(defmacro require-login (&body body)
  (alexandria:with-gensyms (session)
    `(let ((,session (context :session)))
       (if (not (eql nil (gethash :userinfo ,session)))
         (progn
           ,@body)
         (progn
           (setf (gethash :next-page session) (lack.request:request-path-info *request*))
           '(302 (:location "/login")))))))

(defmacro with-session ((var) &body body)
  `(let ((,var (context :session)))
     ,@body))


(def-route "/login/google" (params)
  (with-session (session)
    (let ((state (gen-state 36)))
      (setf (gethash :state session) state)
      (with-endpoints *goog-endpoint-schema*
        (setf (gethash :endpoint-schema session) *goog-endpoint-schema*)
        (multiple-value-bind (content rcode headers) (do-auth-request *goog-endpoint-schema* state)
          (if (< rcode 400)
            `(302 (:location ,(cdr (assoc :location headers))))
            content))))))


(def-route "/login/facebook" (params)
  (with-session (session)
    (let ((state (gen-state 36)))
      (setf (gethash :state session) state)
      (with-endpoints *fbook-endpoint-schema*
        (setf (gethash :endpoint-schema session) *fbook-endpoint-schema*)
        (multiple-value-bind (content rcode headers uri) (do-auth-request *fbook-endpoint-schema* state)
          (if (< rcode 400)
            `(302 (:location ,(format nil "~a" uri)))
            content))))))

(def-route "/oidc_callback/google" (params)
  (let ((received-state (cdr (string-assoc "state" params)))
        (code (cdr (string-assoc "code" params))))
    (check-state received-state
                 (with-session (session)
                   (with-endpoints *goog-endpoint-schema*
                     (let* ((a-t (get-access-token *goog-endpoint-schema* code))
                            (id-token (assoc-cdr :id--token a-t))
                            (decoded (cljwt:decode id-token :fail-if-unsupported nil)))
                       (setf (gethash :userinfo session) decoded)
                       '(302 (:location "/")))))
                 '(403 '() "Out, vile imposter!"))))


(def-route "/oidc_callback/facebook" (params)
  (let ((received-state (cdr (string-assoc "state" params)))
        (code (cdr (string-assoc "code" params))))
    (with-endpoints *fbook-endpoint-schema*
      (check-state received-state
                   (with-session (session)
                     (let* ((a-t (get-access-token *fbook-endpoint-schema* code))
                            (id-token (assoc-cdr :access--token a-t)))
                       (setf (gethash :userinfo session) (get-user-info *fbook-endpoint-schema* id-token))
                       '(302 (:location "/"))))
                   '(403 '() "Out, vile imposter!")))))

(def-route "/userinfo.json" (params)
  (with-session (session)
    (require-login 
      (with-endpoints  (gethash :endpoint-schema session)
        (cl-json:encode-json-to-string (gethash :userinfo session))))))

(def-route "/logout" (params)
  (with-session (session)
    (setf (gethash :userinfo session) nil)
    '(302 (:location "/"))))

(def-route "/login" (params)
  (cl-who:with-html-output-to-string (s)
    (:html
      (:head
        (:title "Login"))
      (:body
        (:div (:a :href "/login/facebook" "Facebook"))
        (:div (:a :href "/login/google" "Google")))))) 

(def-route "/" (params)
  (with-session (session)
    (if (not (null (gethash :next-page session)))
      `(302 (:location ,(gethash :next-page session)))
      (require-login 
        (anaphora:sunless (gethash :counter session) (setf anaphora:it 0))
        (format nil "~Ath visit<br/>~a<br/><br/>~S<br/>"
                (gethash :counter session)
                (alexandria:hash-table-alist session)
                (alexandria:hash-table-alist (context :session)))))))

(setf *handler* (clack:clackup (lack.builder:builder :session *app*) :port 9090))

