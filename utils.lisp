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

(in-package :cl-oid-connect.utils)
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

(defmacro string-assoc (key alist) `(assoc ,key ,alist :test #'equal))
(defmacro assoc-cdr (key alist &optional (test '#'eql)) `(cdr (assoc ,key ,alist :test ,test)))

(defmacro define-auth-entry-point (name endpoint-schema)
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

(defmacro define-auth-callback (name endpoint-schema params &body body)
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

(defmacro ensure-logged-in (&body body)
  "Ensure that the user is logged in: otherwise throw the condition user-not-logged-in"
  (alexandria:with-gensyms (session userinfo)
    `(my-with-context-variables ((,session session))
       (with-session-values ((,userinfo userinfo)) ,session
         (handler-case
           (if (null ,userinfo)
             (error 'user-not-logged-in)
             (progn ,@body))
           (error (c)
             (setf ,userinfo nil)
             (error c)))))))

(defmacro setup-oid-connect (app args &body callback)
  `(bind-oid-connect-routes ,app (lambda ,args ,@callback)))

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

(defmacro redirect-if-necessary (sessionvar &body body)
  (with-gensyms (session)
    `(let* ((,session ,sessionvar)
            (next-page (gethash :next-page ,session)))
       (if (and (not (null next-page))
                (not (string= next-page (lack.request:request-path-info *request*))))
         (progn
           (setf (gethash :next-page ,session) nil)
           `(302 (:location ,next-page)))))))

