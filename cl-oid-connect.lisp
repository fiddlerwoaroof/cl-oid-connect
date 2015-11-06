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

(defun run-callback-function (endpoint-schema params get-app-user-cb get-login-data)
  (flet ((get-code (params) (assoc-cdr "code" params #'equal)))
    (let ((a-t (get-access-token endpoint-schema (get-code params))))
      (multiple-value-bind (access-token user-info id-token) (funcall get-login-data a-t)
        (auth-callback-skeleton params (:endpoint-schema endpoint-schema
                                        :auth-session-vars (accesstoken userinfo idtoken app-user))
          (setf accesstoken access-token
                userinfo user-info
                idtoken id-token
                app-user (funcall get-app-user-cb user-info id-token access-token)))
        '(302 (:location "/"))))))


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
                      #'goog-get-access-token)
  (setf (redirect-uri *goog-endpoint-schema*) "http://srv2.elangley.org:9090/oidc_callback/google"))

(define-auth-entry-point google-login-entry *goog-endpoint-schema*)
(define-auth-entry-point facebook-login-entry *fbook-endpoint-schema*)

(define-auth-callback google-callback *goog-endpoint-schema* (a-t)
  (labels ((get-real-access-token (a-t) (assoc-cdr :access--token a-t))
           (get-id-token (a-t) (cljwt:decode (assoc-cdr :id--token a-t) :fail-if-unsupported nil)))
    (let ((access-token (get-real-access-token a-t)))
      (values access-token
        (get-user-info *goog-endpoint-schema* access-token)
        (get-id-token a-t)))))

(define-auth-callback facebook-callback *fbook-endpoint-schema* (a-t)
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

