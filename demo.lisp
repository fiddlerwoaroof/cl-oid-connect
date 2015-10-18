(in-package :cl-user)
(ql:quickload :clack-middleware-postmodern)

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
(ql:quickload :cl-actors)
(ql:quickload :simple-tasks)

(declaim (optimize (speed 0) (safety 3) (debug 2)))

(push (cons "application" "rdf+xml") drakma:*text-content-types*)
(push (cons "application" "rss+xml") drakma:*text-content-types*)
(push (cons "text" "rss+xml") drakma:*text-content-types*)

;(load "utils.lisp")
(load "rss.lisp")

(defpackage :whitespace
  (:use #:cl #:anaphora #:whitespace.utils #:whitespace.feeds.rss #:whitespace.tables))

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
(defmacro with-whitespace-db (&body body)
  `(postmodern:with-connection *db-connection-info*
     ,@body))
(defmacro wc (&body body)
  `(with-whitespace-db ,@body))

(defmacro with-xml-tags (&body body)
  `(let ((plump:*tag-dispatchers* plump:*xml-tags*))
     ,@body))

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
        (declare (ignorable main-right-margin)) ; TODO: use this!!!
        `(200 (:content-type "text/css") ,ss)))))

(cl-oid-connect:def-route ("/theme/dark.css" (params) :app *app*)
  (colors:let-palette (make-instance 'colors:palette)
    (eval '(get-theme-css))))

(cl-oid-connect:def-route ("/theme/light.css" (params) :app *app*)
  (colors:let-palette (colors:invert-palette (make-instance 'colors:palette))
    (eval '(get-theme-css))))

(cl-oid-connect:def-route ("/userinfo.json" (params) :app *app*)
  (declare (ignore params))
  (ningle:with-context-variables (session)
    (cl-oid-connect:require-login
      (cl-oid-connect::with-endpoints (gethash :endpoint-schema session)
        `(200 (:content-type "application/json") ,(cl-json:encode-json-to-string (gethash :userinfo session)))))))

(cl-oid-connect:def-route ("/logout" (params) :app *app*)
  (declare (ignore params))
  (ningle:with-context-variables (session)
    (setf (gethash :userinfo session) nil)
    '(302 (:location "/"))))

(defun assoc-cdr-alternatives (alist alt1 alt2 &optional (test #'eql))
  (aif (cl-oid-connect:assoc-cdr alt1 alist test)
    it
    (cl-oid-connect:assoc-cdr alt2 alist test)))

(cl-oid-connect::setup-oid-connect *app* (userinfo &rest args)
  (declare (ignore args) (optimize (speed 0) (safety 3) (debug 3)))
  (flet ((get-received-id (userinfo) (assoc-cdr-alternatives userinfo :id :sub))
         (get-db-user (received-id) (car (postmodern:select-dao 'reader_user (:= :foreign-id received-id)))) 
         (get-first-name (userinfo) (assoc-cdr-alternatives userinfo :first--name :given--name))
         (get-last-name (userinfo) (assoc-cdr-alternatives userinfo :last--name :family--name))
         (get-link (userinfo) (assoc-cdr-alternatives userinfo :link :profile)))

    (postmodern:with-transaction ()
      (let ((received-id (get-received-id userinfo)))
        (aif (get-db-user received-id) it
          (postmodern:make-dao
            'reader_user
            :foreign-id received-id
            :first-name (get-first-name userinfo)
            :last-name (get-last-name userinfo)
            :name (cl-oid-connect:assoc-cdr :name userinfo)
            :email (cl-oid-connect:assoc-cdr :email userinfo)
            :gender (cl-oid-connect:assoc-cdr :gender userinfo)
            :link (get-link userinfo)
            :locale (cl-oid-connect:assoc-cdr :locale userinfo)))))))

(defun update-feed (url)
  (with-whitespace-db
    (postmodern:with-transaction ()
      (upsert-feed (make-rss-feed (with-xml-tags (plump:parse (drakma:http-request url))))))))

(defmacro amapcar-with-body (list &body forms)
  (alexandria:once-only (list)
    `(mapcar (lambda (it) ,@forms)
             ,list)))

(defun update-all-feeds ()
  (with-whitespace-db
    (let ((urls (postmodern:query (:select 'fetch-url :from 'rss-feed-store))))
      (amapcar-with-body urls
        (restart-case
          (apply #'update-feed it)
          (continue-updates () (warn (format nil "Skipping feed with fetch-url: ~s" it)))
          (use-value (v) (update-feed v)))))))

(defun minutes (minutes) (* minutes 60))

(defun continue-updates (e)
  (declare (ignore e))
  (let ((restart (find-restart 'continue-updates)))
    (when restart
      (format t "continuing")
      (invoke-restart restart))))

(let (update-thread stop)
  (defun start-update-thread ()
    (setf update-thread
          (bordeaux-threads:make-thread
            (lambda ()
              (loop
                (handler-bind ((drakma:parameter-error #'continue-updates))
                  (update-all-feeds))
                (sleep (ubiquitous:value 'update-frequency))
                (when stop
                  (return-from nil nil))))
            :name "Whitespace Update Thread")))
  (defun stop-update-thread ()
    (setf stop t)
    (setf update-thread nil)))

(let ((handler nil))
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
                ;:csrf
                (lambda (app) (lambda (env)
                                (postmodern:with-connection *db-connection-info*
                                                            (funcall app env))))
                (:static :path "/static/" :root #p"./static/")
                *app*) :port 9090 :server server)
            handler)))

  (defun restart-clack ()
    (do () ((null handler)) (stop))
    (start)))


; vim: foldmethod=marker foldmarker=(,) foldminlines=3 :
