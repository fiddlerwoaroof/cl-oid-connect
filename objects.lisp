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

(in-package :cl-oid-connect.objects)

(setf drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))

(defparameter =service-info=
  (object :parents '()
          :properties '((client-id nil :accessor client-id)
                        (secret nil :accessor secret))))
(defparameter *fbook-info* (clone =service-info=))
(defparameter *goog-info* (clone =service-info=))
(defparameter *endpoint-schema* nil)

(defparameter =endpoint-schema=
  (object :parents '()
          :properties '((auth-endpoint nil :accessor auth-endpoint)
                        (token-endpoint nil :accessor token-endpoint)
                        (userinfo-endpoint nil :accessor t)
                        (auth-scope "openid profile email" :accessor t)
                        (redirect-uri nil :accessor t))))

(defmessage get-user-info (a b))
(defmessage get-access-token (a b))
(defmessage discover-endpoints (a b c))

(defreply get-user-info ((a =endpoint-schema=) (b =string=)))
(defreply get-access-token ((a =endpoint-schema=) (b =string=)))

(defreply discover-endpoints ((schema =endpoint-schema=) discovery-doc-url get-access-token)
  "Discover endpoints on the basis of a discovery document stored at a particular url.
   The two keyword arguments define a function to bind to sheeple replies for get-user-token
   and get-access-token."
  (let ((discovery-document (yason:parse (drakma:http-request discovery-doc-url))))
    (setf (auth-endpoint schema)     (gethash "authorization_endpoint" discovery-document)
          (token-endpoint schema)    (gethash "token_endpoint" discovery-document)
          (userinfo-endpoint schema) (gethash "userinfo_endpoint" discovery-document))
    (defreply get-access-token ((a schema) (b =string=))
      (funcall get-access-token a b))

    schema))

(defparameter *goog-endpoint-schema* (defobject (=endpoint-schema= *goog-info*)))

(defreply get-user-info ((endpoint-schema *goog-endpoint-schema*) (access-token =string=))
  (format t "getting user data: ~a~%" "blarg")
  (let ((endpoint (userinfo-endpoint endpoint-schema)))
    (cl-json:decode-json-from-string
      (drakma:http-request endpoint
                           :parameters `(("alt" . "json")
                                         ("access_token" . ,access-token))))))


(defproto *fbook-endpoint-schema* (=endpoint-schema= *fbook-info*)
          ((auth-endpoint "https://www.facebook.com/dialog/oauth")
           (token-endpoint "https://graph.facebook.com/v2.3/oauth/access_token")
           (userinfo-endpoint "https://graph.facebook.com/v2.3/me")
           (auth-scope "email")
           (redirect-uri  "http://srv2.elangley.org:9090/oidc_callback/facebook")))


(defreply get-access-token ((endpoint-schema *fbook-endpoint-schema*) (code =string=))
  (cl-json:decode-json-from-string
    (drakma:http-request (token-endpoint endpoint-schema)
                         :method :post
                         :redirect nil
                         :parameters `(("code" . ,code)
                                       ("client_id" . ,(client-id endpoint-schema))
                                       ("app_id" . ,(client-id endpoint-schema))
                                       ("client_secret" . ,(secret endpoint-schema))
                                       ("redirect_uri" . ,(redirect-uri endpoint-schema))
                                       ("grant_type" . "authorization_code")))))

(defreply get-user-info ((endpoint-schema *fbook-endpoint-schema*) (access-token =string=))
  (let ((endpoint (userinfo-endpoint endpoint-schema)))
    (cl-json:decode-json-from-string
      (drakma:http-request endpoint
                           :parameters `(("access_token" . ,access-token))))))

(defun do-auth-request (endpoint-schema state)
  (drakma:http-request (auth-endpoint endpoint-schema)
                       :redirect nil
                       :parameters `(("client_id" . ,(client-id endpoint-schema))
                                     ("app_id" . ,(client-id endpoint-schema))
                                     ("response_type" . "code")
                                     ("scope" . ,(auth-scope endpoint-schema))
                                     ("redirect_uri" . ,(redirect-uri endpoint-schema))
                                     ("state" . ,state))))


