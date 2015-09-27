(in-package :cl-user)
(ql:quickload :cl-markup)
(ql:quickload :cl-oid-connect)
(ql:quickload :colors)
(ql:quickload :lquery)
(ql:quickload :plump)
(ql:quickload :postmodern)
(ql:quickload :sxql)
(ql:quickload :clack-middleware-postmodern)
(ql:quickload :dexador)
(ql:quickload :spinneret)
(ql:quickload :ubiquitous)
(ql:quickload :iterate)
(ql:quickload :jonathan)

(declaim (optimize (speed 0) (safety 3) (debug 2)))

(push (cons "application" "rdf+xml") drakma:*text-content-types*)
(push (cons "application" "rss+xml") drakma:*text-content-types*)
(push (cons "text" "rss+xml") drakma:*text-content-types*)

(load "rss.lisp")


(defpackage :whitespace
  (:use #:cl #:whitespace.utils #:whitespace.rss #:whitespace.tables))

(in-package plump-dom)

(defmethod serialize-object :around ((node element))
  (when (string= (tag-name node) "iframe")
    (make-text-node node))
  (call-next-method))


(in-package :whitespace)
(ubiquitous:restore :whitespace)


(defparameter *app* (make-instance 'ningle:<app>))

(cl-oid-connect:def-route ("/login" (params) :app *app*)
  (cl-who:with-html-output-to-string (s)
    (:html
      (:head (:title "Login"))
      (:body
        (:div
          :class "login-button facebook"
          (:a :href "/login/facebook" "Facebook"))
        (:div
          :class "login-button google"
          (:a :href "/login/google" "Google"))))))


(handler-bind ((warning #'sb-ext::muffle-warning))
  (let* ((feed-urls (ubiquitous:value 'feed 'urls))
         (plump-parser:*tag-dispatchers* plump:*xml-tags*)
         (docs (map 'vector (lambda (x) (plump:parse (drakma:http-request x))) feed-urls)))
    (defparameter *feeds* (map 'vector (lambda (x) (make-rss-feed x)) docs))))

(defparameter *db-connection-info* (ubiquitous:value 'db 'connection 'info))

(defmacro def-markup (name (&rest args) &body body)
  `(defmacro ,name ,args
     (alexandria:once-only ,args
      `(cl-markup:markup
         ,,@body))))

(defmacro item-markup (item)
  (alexandria:once-only (item)
    `(let ((,item ,item))
       (cl-markup:markup
         (:li :class "link closed"
          (:section :class "link-header"
           (:h4 (rss-item-title ,item))
           (:p :class "link-info"
            (:a :target "_blank" :href (rss-item-link ,item)
             (:span :class "link-url" (rss-item-link ,item)))
            (:span :class "link-date") (rss-item-pub-date ,item)))
          (:section :class "link-content"
           (:div
             (cl-markup:raw (rss-item-description-raw ,item)))))))))

(defmacro feed-markup (feed-v fc-v)
  (alexandria:with-gensyms (feed fc)
    `(let ((,feed ,feed-v)
           (,fc ,fc-v))
       (cl-markup:markup
         (:section :class "feed closed" :id (format nil "feed-~a" ,fc)
          (:section :class "feed-header"
           (:h2 (rss-feed-title ,feed))
           (:h3 (rss-feed-description ,feed)))
          (:ul :class "post-list"
           (:li :class "link closed" :ng-repeat (format nil "item in feeds.result[~d].items" (- ,fc 1))
            (:section :class "link-header"
             (:h4 "{{item.title}}")
             (:p :class "link-info"
              (:a :target "_blank" :ng-href "{{item.link}}" :class "link-url" "{{item.link}}")
              (:span :class "link-date" "{{item.date}}")))
            (:section :class "link-content"
             (:div
               "{{item.description}}")))
           ))))))

(defmacro feedlist-markup (feedlist)
  (alexandria:once-only (feedlist)
    `(cl-markup:markup*
       `(:ul :class "menu"
         ,@(loop for feed across ,feedlist
                 count feed into feed-count
                 collect
                 (list :li
                       (list :a
                             :href (format nil "#feed-~a" feed-count)
                             (rss-feed-title feed))))))))

(load "base-template.lisp")

(defmacro defun-from-value (name value)
  `(setf (symbol-function ',name) ,value))

(defun-from-value jsonapi-encoder
                  (jonathan.helper:compile-encoder () (success result)
                    (list :|success| success
                          :|result| result)))

(defmacro neither (&rest forms) `(not (or ,@forms)))
(defmacro neither-null (&rest forms)
  `(neither ,@(loop for form in forms collecting `(null ,form))))

; ; ;  Ultimately, this will only serialize the feed if the client
(cl-oid-connect:def-route ("/feeds/add" (params) :method :post :app *app*)
  (ningle.context:with-context-variables (session) 
    (let ((user-info (gethash :app-user session))
          (result '(302 (:location "/")))
          (api (string= (cl-oid-connect:assoc-cdr "api" params 'string=) "yes")) 
          (url (cl-oid-connect:assoc-cdr "url" params 'string=)) 
          (plump-parser:*tag-dispatchers* plump-parser:*xml-tags*))
      (cl-oid-connect:require-login
        (when (neither-null params user-info)
          (handler-case
            (let* ((doc (plump:parse (drakma:http-request url)))
                   (uid (slot-value user-info 'id)))
              (multiple-value-bind (added-feed dao-feed) (store-feed doc) 
                (subscribe-to-feed uid (slot-value dao-feed 'id))
                (when api
                  (setf result `(200 (:Content-Type "application/json") ,(jsonapi-encoder t added-feed))))))
            (cl-postgres-error:unique-violation
              ()
              (when api
                (setf result
                      `(400 () ,(jsonapi-encoder nil "Feed already saved"))))))))
      result)))

;;; TODO: add needs to return the new content, so that angular can append it

(cl-oid-connect:def-route ("/feeds/json" (params) :app *app*)
  (ningle.context:with-context-variables (session)
    (let* ((user-info (gethash :app-user session))
           (*feeds* (if user-info (deserialize user-info) *feeds*)))
      `(200 (:content-type "application/json" :cache-control "private, max-age=300") ,(jsonapi-encoder t *feeds*)))))

(cl-oid-connect:def-route ("/feeds/:feeds/html" (params) :app *app*)
  (ningle.context:with-context-variables (session)
    (cl-oid-connect:require-login
      (let* ((feedlist-s (cdr (assoc :feeds params)))
             (feedlist (mapcar #'parse-integer (split-sequence:split-sequence #\SPACE feedlist-s)))
             (*feeds* (gethash :feeds session *feeds*))
             (*feeds* (make-array (list (length feedlist))
                                  :initial-contents (loop for x in feedlist
                                                          collect (elt *feeds* x)))))
        (base-template-f)))))

(defun login-callback (userinfo &rest args)
  (declare (ignore args))
  (postmodern:with-transaction ()
    (let* ((received-id (anaphora:aif (cl-oid-connect:assoc-cdr :id userinfo)
                          anaphora:it
                          (cl-oid-connect:assoc-cdr :sub userinfo)))
           (db-user (car (postmodern:select-dao 'reader_user (:= :foreign-id received-id)))))
      (if (not (null db-user))
        db-user
        (progn
          (let ((name (cl-oid-connect:assoc-cdr :name userinfo))
                (first-name (anaphora:aif (cl-oid-connect:assoc-cdr :first--name userinfo)
                              anaphora:it
                              (cl-oid-connect:assoc-cdr :given--name userinfo)))
                (last-name (anaphora:aif (cl-oid-connect:assoc-cdr :last--name userinfo)
                             anaphora:it
                             (cl-oid-connect:assoc-cdr :family--name userinfo)))
                (email (cl-oid-connect:assoc-cdr :email userinfo))
                (gender (cl-oid-connect:assoc-cdr :gender userinfo))
                (link (anaphora:aif (cl-oid-connect:assoc-cdr :link userinfo)
                        anaphora:it
                        (cl-oid-connect:assoc-cdr :profile userinfo)))
                (locale (cl-oid-connect:assoc-cdr :locale userinfo)))
            (postmodern:make-dao 'reader_user
                                 :foreign-id received-id
                                 :first-name first-name
                                 :last-name last-name
                                 :name name
                                 :email email
                                 :gender gender
                                 :link link
                                 :locale locale)))))))

(cl-oid-connect:def-route ("/demo" (params) :app *app*)
  (base-template-f t))

(cl-oid-connect:def-route ("/" (params) :app *app*)
  (ningle:with-context-variables (session)
    (cl-oid-connect:require-login
      (cl-oid-connect:redirect-if-necessary session
        (let* ((user-info (gethash :app-user session))
               (*feeds* (deserialize user-info)))
          (base-template-f))))))

;;; this will be bound by calls to with-palette
;;; probably should be refactored out
(defparameter *palette* nil)

(defparameter *colorscheme* (make-instance 'colors:colorscheme))

(defun get-theme-css ()
  (colors:with-palette (*palette*)
    (flet ((combine-unit-q (quant unit) (format nil "~d~a" quant unit)))
      (let* ((header-height 9)
             (main-right-margin (* 0.618 (- 100 header-height)))
             (height-units "vh")
             (ss (lass:compile-and-write
                   `(* :color ,(colors:colorscheme-fg *colorscheme*))

                   `(body
                      :background-color ,(colors:colorscheme-bg *colorscheme*))

                   `((:or h1 h2 h3)
                     :color ,(colors:colorscheme-fg-highlight *colorscheme*))
                   `(.feed-header
                      :background-color ,(colors:colorscheme-bg-highlight *colorscheme*))

                   `((:or h4 h5 h6) :color ,(colors:colorscheme-fg-highlight *colorscheme*))

                   `(header
                      :border-bottom "thin" "solid" ,(colors:colorscheme-accent *colorscheme*)
                      :height ,(combine-unit-q header-height height-units)
                      :font-size ,(combine-unit-q (* 0.75 header-height) height-units)
                      :line-height ,(combine-unit-q header-height height-units)
                      (.flip-button
                        :background-color ,(colors:colorscheme-fg *colorscheme*)
                        :color ,(colors:colorscheme-bg *colorscheme*))
                      ((:and .flip-button :focus)
                       :outline none)
                      ((:and .flip-button :hover)
                       :font-size ,(combine-unit-q (* 0.25 header-height) height-units)))

                   `(main
                      :border-left medium solid ,(colors:colorscheme-accent *colorscheme*)
                      :height ,(combine-unit-q (- 100 header-height) height-units)
                      ("#add-form"
                        :box-shadow "0em" "0em" "0.2em" "0.2em" ,(colors:colorscheme-accent *colorscheme*)
                        ((:or input button)
                         :background-color ,(colors:colorscheme-bg *colorscheme*)
                         :color ,(colors:colorscheme-fg *colorscheme*))
                        )
                      )

                   `((:or a (:and a :visited) (:and a :active) code.url)
                     :color ,(colors:colorscheme-fg-highlight *colorscheme*))

                   `(section#sidebar
                      :transition opacity "0.5s" ease
                      (ul.menu
                        ((li + li)
                         :border-top "thin" "solid" ,(colors:colorscheme-fg-highlight *colorscheme*))
                        ((:and li :hover)
                         :background-color ,(colors:colorscheme-hover-highlight *colorscheme*)
                         :color ,(colors:colorscheme-fg-highlight *colorscheme*))))

                   `(.feed
                      :border-bottom thick solid ,(colors:colorscheme-accent *colorscheme*)
                      :border-left none)

                   `(.link-header :background-color ,(colors:colorscheme-bg-highlight *colorscheme*))
                   `(.link
                      :border-top thin solid ,(colors:colorscheme-fg *colorscheme*)
                      :border-bottom none


                      (.link-info
                        :color ,(colors:colorscheme-fg-deemph *colorscheme*)
                        :border-bottom "thin" "solid" ,(colors:colorscheme-fg *colorscheme*)
                        ((:or a span)
                         :color inherit)
                        ((:and a :hover)
                         :color ,(colors:colorscheme-fg *colorscheme*))
                        ))

                   `((:and .feed-header :hover)
                     :background-color ,(colors:colorscheme-hover-highlight *colorscheme*))

                   `((.link.closed .link-header)
                     :background-color ,(colors:colorscheme-bg *colorscheme*))

                   `((:or (:and .link-header :hover) (.link.closed (:and .link-header :hover)))
                     :background-color ,(colors:colorscheme-hover-highlight *colorscheme*)))))
        `(200 (:content-type "text/css") ,ss)))))

(cl-oid-connect:def-route ("/theme/dark.css" (params) :app *app*)
  (colors:let-palette (make-instance 'colors:palette)
    (eval '(get-theme-css))))

(cl-oid-connect:def-route ("/theme/light.css" (params) :app *app*)
  (colors:let-palette (colors:invert-palette (make-instance 'colors:palette))
    (eval '(get-theme-css))))

(defvar *handler* nil)

(defun stop ()
  (clack:stop (pop *handler*)))

(ql:quickload :clack-middleware-postmodern)

(defparameter oid-mw
  (cl-oid-connect:oauth2-login-middleware
    *app*
    :facebook-info (truename "~/github_repos/cl-oid-connect/facebook-secrets.json")
    :google-info (truename "~/github_repos/cl-oid-connect/google-secrets.json")
    :login-callback #'login-callback))

(defun start (&optional tmp)
  (let ((server (if (> (length tmp) 1)
                  (intern (string-upcase (elt tmp 1)) 'keyword)
                  :hunchentoot)))
    (push (clack:clackup
            (lack.builder:builder
              :backtrace
              :session
              ;:csrf
              (lambda (app) (lambda (env)
                              (postmodern:with-connection *db-connection-info*
                                (funcall app env))))
              (:static :path "/static/" :root #p"./static/")
              *app*) :port 9090 :server server)
          *handler*)))


(defun restart-clack ()
  (do () ((null *handler*))
    (stop))
  (start))

; vim: foldmethod=marker foldmarker=(,) foldminlines=3 :
