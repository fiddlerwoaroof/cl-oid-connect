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

(with-open-file (goog-info #P"google-secrets.json")
  (let* ((data (yason:parse goog-info))
         (client-id (gethash "client-id" data))
         (secret (gethash "secret" data)))
    (defconstant *GOOG-CLIENT-ID* client-id)
    (defconstant *GOOG-CLIENT-SECRET* secret)))

;;; "cl-oid-connect" goes here. Hacks and glory await!
(defvar *app* (make-instance 'ningle:<app>))
(defvar *state* nil)

;; These tokens specify the auth endpoint. These are autodiscovered, if the relevant
;; functions are wrapped with the "with-goog-endpoints" macro.
(defvar *auth-endpoint* nil)
(defvar *token-endpoint* nil)

(defmacro string-assoc (key alist) `(assoc ,key ,alist :test #'equal))
(defmacro assoc-cdr (key alist) `(cdr (assoc ,key ,alist)))

(defmacro with-goog-endpoints (&body body)
  (alexandria:with-gensyms (discovery-document)
    `(let* ((,discovery-document
              (cl-json:decode-json-from-string
                (drakma:http-request "https://accounts.google.com/.well-known/openid-configuration")))
            (*auth-endpoint* (assoc-cdr :authorization--endpoint ,discovery-document))
            (*token-endpoint* (assoc-cdr :token--endpoint ,discovery-document)))
       ,@body)))     

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
                                       ("client_id" . ,*GOOG-CLIENT-ID*)
                                       ("client_secret" . ,*GOOG-CLIENT-SECRET*)
                                       ("redirect_uri" . "http://srv2.elangley.org:9090/oidc_callback")
                                       ("grant_type" . "authorization_code")))))

(defun do-auth-request (state)
  (drakma:http-request *auth-endpoint*
                       :redirect nil
                       :parameters `(("client_id" . ,*GOOG-CLIENT-ID*)
                                     ("response_type" . "code")
                                     ("scope" . "openid email")
                                     ("redirect_uri" . "http://srv2.elangley.org:9090/oidc_callback")
                                     ("state" . ,state))))

(defun gen-state (len)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat len
            do (princ (random 36) stream)))))

(defmacro def-route (url args &body body)
  `(setf (ningle:route *app* ,url)
         #'(lambda ,args
             ,@body)))

(defmacro check-state (received-state then else)
  (alexandria:with-gensyms (saved-state)
    `(let ((,saved-state (gethash :state *session*)))
       (if (equal ,saved-state ,received-state)
         ,then
         ,else))))

(defmacro require-login (&body body)
  `(if (not (eql nil (gethash :userinfo *session*)))
     (progn
       ,@body)
     '(302 (:location "/login"))))

(def-route "/login" (params)
  (declare (ignore params))
  (let ((state (gen-state 36)))
    (setf (gethash :state *session*) state)
    (multiple-value-bind (content rcode headers) (do-auth-request state)
      (if (< rcode 400)
        `(302 (:location ,(cdr (assoc :location headers))))
        content))))

(def-route "/oidc_callback" (params)
  (let ((received-state (cdr (string-assoc "state" params)))
        (code (cdr (string-assoc "code" params))))
    (check-state received-state
                 (let* ((a-t (get-access-token code)) (id-token (assoc-cdr :id--token a-t))
                        (decoded (cljwt:decode id-token :fail-if-unsupported nil)))
                   (setf (gethash :userinfo *session*) decoded)
                   '(302 (:location "/")))
                 '(403 '() "Out, vile imposter!"))))

(def-route "/" (params)
  (require-login 
    (anaphora:sunless (gethash :counter *session*) (setf anaphora:it 0))
    (format nil "~Ath visit<br/>~a<br/>~S"
            (incf (gethash :counter *session*))
            *state*
            (alexandria:hash-table-alist *session*))))



(setf *handler* (clack:clackup (lack.builder:builder :session *goog-mw* *app*)
                               :port 9090))

