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
(setq drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))

(with-open-file (fbook-info #P"facebook-secrets.json")
  (let* ((data (yason:parse fbook-info))
         (client-id (gethash "client-id" data))
         (secret (gethash "secret" data)))
    (defvar *FBOOK-CLIENT-ID* client-id)
    (defvar *FBOOK-CLIENT-SECRET* secret)))

(with-open-file (goog-info #P"google-secrets.json")
  (let* ((data (yason:parse goog-info))
         (client-id (gethash "client-id" data))
         (secret (gethash "secret" data)))
    (defvar *GOOG-CLIENT-ID* client-id)
    (defvar *GOOG-CLIENT-SECRET* secret)))

;;; "cl-oid-connect" goes here. Hacks and glory await!
(defvar *app* (make-instance 'ningle:<app>))
(defvar *state* nil)

;; These tokens specify the auth endpoint. These are autodiscovered, if the relevant
;; functions are wrapped with the "with-goog-endpoints" macro.
(defvar *auth-endpoint* nil)
(defvar *token-endpoint* nil)
(defvar *client-id* nil)
(defvar *client-secret* nil)
(defvar *user-info-cb* (lambda ()))
(defvar *get-access-token* (lambda ()))
(defvar *redirect-uri* nil)

(defmacro string-assoc (key alist) `(assoc ,key ,alist :test #'equal))
(defmacro assoc-cdr (key alist) `(cdr (assoc ,key ,alist)))

(defmacro with-goog-endpoints (&body body)
  (alexandria:with-gensyms (discovery-document)
    `(let* ((,discovery-document
              (cl-json:decode-json-from-string
                (drakma:http-request "https://accounts.google.com/.well-known/openid-configuration")))
            (*auth-endpoint* (assoc-cdr :authorization--endpoint ,discovery-document))
            (*token-endpoint* (assoc-cdr :token--endpoint ,discovery-document))
            (*client-id* *GOOG-CLIENT-ID*)
            (*client-secret* *GOOG-CLIENT-SECRET*)
            (*redirect-uri* "http://srv2.elangley.org:9090/oidc_callback/google")
            )
       ,@body)))     


(defmacro with-fbook-endpoints (&body body)
  `(let* ((*auth-endpoint* "https://www.facebook.com/dialog/oauth")
          (*token-endpoint* "https://graph.facebook.com/v2.3/oauth/access_token")
          (*client-id* *FBOOK-CLIENT-ID*)
          (*client-secret* *FBOOK-CLIENT-SECRET*)
          (*user-info-cb* #'fb-get-userinfo)
          (*get-access-token* #'fb-get-access-token)
          (*redirect-uri* "http://srv2.elangley.org:9090/oidc_callback/facebook"))
     ,@body))     

(defun fb-get-userinfo (access-token)
  (let ((endpoint "https://graph.facebook.com/v2.3/me"))
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

(defun get-access-token (code)
  (cl-json:decode-json-from-string
    (drakma:http-request *token-endpoint*
                         :method :post
                         :redirect nil
                         :parameters `(("code" . ,code)
                                       ("client_id" . ,*client-id*)
                                       ("app_id" . ,*client-id*)
                                       ("client_secret" . ,*client-secret*)
                                       ("redirect_uri" . ,*redirect-uri*)
                                       ("grant_type" . "authorization_code")))))

(defun do-auth-request (state)
  (format t "~%client-id: ~a~%" *client-id*)
  (drakma:http-request *auth-endpoint*
                       :redirect nil
                       :parameters `(("client_id" . ,*client-id*)
                                       ("app_id" . ,*client-id*)
                                     ("response_type" . "code")
                                     ("scope" . "email")
                                     ("redirect_uri" . ,*redirect-uri*)
                                     ("state" . ,state))))

(defun gen-state (len)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat len
            do (princ (random 36) stream)))))

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
         '(302 (:location "/login"))))))

(defmacro with-session ((var) &body body)
  `(let ((,var (context :session)))
     ,@body))


(def-route "/login" (params)
  (cl-who:with-html-output-to-string (s)
    (:html
      (:head
        (:title "Login"))
      (:body
        (:div (:a :href "/login/facebook" "Facebook"))
        (:div (:a :href "/login/google" "Google")))))) 


(def-route "/login/google" (params)
  (with-session (session)
    (let ((state (gen-state 36)))
      (setf (gethash :state session) state)
      (with-goog-endpoints 
        (multiple-value-bind (content rcode headers) (do-auth-request state)
          (if (< rcode 400)
            `(302 (:location ,(cdr (assoc :location headers))))
            content))))))


(def-route "/login/facebook" (params)
  (with-session (session)
    (let ((state (gen-state 36)))
      (setf (gethash :state session) state)
      (with-fbook-endpoints 
        (multiple-value-bind (content rcode headers uri) (do-auth-request state)
          (if (< rcode 400)
            `(302 (:location ,(format nil "~a" uri)))
            content))))))

;(def-route "/oidc_callback" (params)
;  (let ((received-state (cdr (string-assoc "state" params)))
;        (code (cdr (string-assoc "code" params))))
;    (with-fbook-endpoints
;      (check-state received-state
;                   (let* ((a-t (get-access-token code)))
;                     (format nil "~s" a-t))
;                   '(403 '() "Out, vile imposter!")))))

(def-route "/oidc_callback/google" (params)
  (let ((received-state (cdr (string-assoc "state" params)))
        (code (cdr (string-assoc "code" params))))
    (check-state received-state
                 (with-session (session)
                   (with-goog-endpoints 
                     (let* ((a-t (get-access-token code)) (id-token (assoc-cdr :id--token a-t))
                            (decoded (cljwt:decode id-token :fail-if-unsupported nil)))
                       (setf (gethash :userinfo session) decoded)
                       '(302 (:location "/")))))
                 '(403 '() "Out, vile imposter!"))))


(def-route "/oidc_callback/facebook" (params)
  (let ((received-state (cdr (string-assoc "state" params)))
        (code (cdr (string-assoc "code" params))))
    (with-fbook-endpoints 
      (check-state received-state
                   (with-session (session)
                     (let* ((a-t (get-access-token code))
                            (id-token (assoc-cdr :access--token a-t)))
                       (setf (gethash :userinfo session) (funcall *user-info-cb* id-token))
                       '(302 (:location "/"))))
                   '(403 '() "Out, vile imposter!")))))

(def-route "/userinfo.json" (params)
  (with-session (session)
    (require-login 
      (with-fbook-endpoints 
        (cl-json:encode-json-to-string (gethash :userinfo session))))))

(def-route "/logout" (params)
  (with-session (session)
    (setf (gethash :userinfo session) nil)
    '(302 (:location "/"))))

(def-route "/" (params)
  (with-session (session)
    (require-login 
      (anaphora:sunless (gethash :counter session) (setf anaphora:it 0))
      (format nil "~Ath visit<br/>~a<br/><br/>~S<br/>"
              (gethash :counter session)
              (alexandria:hash-table-alist session)
              (alexandria:hash-table-alist (context :session))))))



(setf *handler* (clack:clackup (lack.builder:builder :session *app*) :port 9090))

