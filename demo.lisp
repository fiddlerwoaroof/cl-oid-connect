(in-package :cl-user)
(ql:quickload :cl-markup)
(ql:quickload :cl-oid-connect)
(ql:quickload :colors)
(ql:quickload :lquery)
(ql:quickload :plump)
(ql:quickload :postmodern)

(push (cons "application" "rdf+xml") drakma:*text-content-types*)
(push (cons "application" "rss+xml") drakma:*text-content-types*)
(push (cons "text" "rss+xml") drakma:*text-content-types*)

(lquery:define-lquery-list-function tag-name (nodes &rest tags)
  "Manipulate elements on the basis of there tag-name.
   With no arguments, return their names else return
   the corresponding tags."
  (if (null tags)
    (map 'vector #'plump:tag-name nodes)
    (apply #'vector
           (loop for node across nodes
                 if (find (plump:tag-name node) tags :test #'string=)
                 collect node))))

(defparameter *app* (make-instance 'ningle:<app>))

(defclass rss-feed ()
  ((feed :accessor rss-feed-feed :initarg :feed)
   (channel :accessor rss-feed-channel)
   (title :accessor rss-feed-title)
   (link :accessor rss-feed-link)
   (description :accessor rss-feed-description)
   (items :accessor rss-feed-items)))

(defgeneric serialize (cls))


(defclass rss-item ()
  ((item :accessor rss-item-item  :initarg :item)
   (title :accessor rss-item-title)
   (link :accessor rss-item-link)
   (description-raw :accessor rss-item-description-raw)
   (description :accessor rss-item-description)
   (category :accessor rss-item-category)
   (comments :accessor rss-item-comments)
   (enclosure :accessor rss-item-enclosure)
   (guid :accessor rss-item-guid)
   (pub-date :accessor rss-item-pub-date)
   (source :accessor rss-item-source)))

(defmethod serialize ((obj rss-feed))
  (postmodern:make-dao 'rss)
  )

(defmacro get-elements (feed &optional (filter nil))
  (let ((feed-sym (gensym))
        (filter-lis `(lambda (x) (and (plump-dom:element-p x) ,@(loop for x in filter
                                                                      collect `(funcall ,x x))))))
    `(let ((,feed-sym ,feed))
       (remove-if-not ,filter-lis (plump:children ,feed-sym)))))

(defmacro get-elements-by-tagname (feed tagname)
  `(get-elements ,feed ((lambda (x) (string= ,tagname (plump:tag-name x))))))

(defmacro extract-text (selector &optional (default ""))
  `(or (lquery:$ ,selector (text) (node)) ,default))

(defun make-rss-item (item)
  (lquery:initialize item)
  (flet ((dehtml (h) (plump:text (plump:parse h)))
         (get-category-names (it) ;;; TODO: simplify this---Ask Shinmera on IRC
           (if (not (equalp #() it))
             (map 'vector
                  (lambda (x) (plump:text (elt (plump:children x) 0)))
                  it)
             #())))
    (let* ((result (make-instance 'rss-item :item item))
           (title (extract-text "title"))
           (link (extract-text "link"))
           (content-encoded (lquery:$ (children) (tag-name "content:encoded")))

           (description-element (if (= 0 (length content-encoded))
                                  (lquery:$ (children "description"))
                                  content-encoded))

           (description-raw (let ((plump:*html-tags*)
                                  (ss (make-string-output-stream)))
                              (if (= 0 (length description-element))
                                ""
                                (progn
                                  (plump:serialize (plump:parse (plump:text (elt description-element 0))) ss)
                                  (get-output-stream-string ss)))))

           (description-munged (dehtml (extract-text "description")))
           (category (get-category-names (lquery:$ "category")))
           ;(comments)
           ;(enclosure)
           (guid (extract-text "aaguid"))
           (pub-date (extract-text "pubDate"))
           (source (extract-text "source")))
      (setf (rss-item-title result) title)
      (setf (rss-item-link result) link)
      (setf (rss-item-description-raw result) description-raw)
      (setf (rss-item-description result) description-munged)
      (setf (rss-item-category result) category)
      ;(setf (rss-item-comments result) comment)
      ;(setf (rss-item-enclosure result) enclosur)
      (setf (rss-item-guid result) guid)
      (setf (rss-item-pub-date result) pub-date)
      (setf (rss-item-source result) source)
      result)))

(defun make-rss-feed (feed)
  (lquery:initialize feed)
  (let* ((result (make-instance 'rss-feed :feed feed))
         (channel (lquery:$ "channel" (node)))
         (title (lquery:$  "title" (text) (node)))
         (link (lquery:$ "link" (text) (node)))
         (description (lquery:$ "description" (text) (node)))
         (items (lquery:$ "item")))
    (setf (rss-feed-channel result) channel)
    (setf (rss-feed-title result) title)
    (setf (rss-feed-link result) link)
    (setf (rss-feed-description result) description)
    (setf (rss-feed-items result)
          (loop for it across items
                collect (make-rss-item it)))
    result))


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

(defparameter *feed-urls*
  #(
    "http://www.reddit.com/r/lisp.rss"
    "http://www.reddit.com/r/scheme.rss"
    "http://www.reddit.com/r/prolog.rss"
    "http://www.reddit.com/r/haskell.rss"
    "http://www.reddit.com/r/talesfromtechsupport.rss"
    "https://drmeister.wordpress.com/feed/rss"
    ))

(let
  ((plump-parser:*tag-dispatchers* plump-parser:*xml-tags*))
  (defparameter *docs* (map 'vector
                            (lambda (x)
                              (format t "~a~%" x)
                              (unwind-protect (plump-parser:parse
                                                (drakma:http-request x))))
                            *feed-urls*)))

(defparameter *feeds* (map 'vector (lambda (x) (unwind-protect (make-rss-feed x))) *docs*))


(defmacro item-markup (item)
  (alexandria:with-gensyms (item-s)
    `(let ((,item-s ,item))
       (cl-markup:markup
         (:li :class "link closed"
          (:section :class "link-header"
           (:h4 (rss-item-title ,item-s))
           (:p :class "link-info"
            (:a :target "_blank" :href (rss-item-link ,item-s)
             (:span :class "link-url" (rss-item-link ,item-s)))
            (:span :class "link-date") (rss-item-pub-date ,item-s)))
          (:section :class "link-content"
           (:div
             (cl-markup:raw (rss-item-description-raw ,item-s)))))))))

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
           (loop for item in (rss-feed-items ,feed)
                 collect (item-markup item))))))))

(defmacro feedlist-markup (feedlist-v)
  (alexandria:with-gensyms (feedlist)
    `(let ((,feedlist ,feedlist-v))
       (cl-markup:markup*
         `(:ul :class "menu"
           ,@(loop for feed across ,feedlist
                   count feed into feed-count
                   collect
                   (list :li
                         (list :a
                               :href (format nil "#feed-~a" feed-count)
                               (rss-feed-title feed)))))))))

(defmacro base-template ()
  `(cl-markup:html5
     (:head
       (:title "My Feeds")
       (:script :src "https://code.jquery.com/jquery-2.1.4.min.js" :type "text/javascript" "")
       (:script :src "/static/js/fold.js" :type "text/javascript" "")
       (:link :rel "stylesheet" :href "/static/css/main.css")
       (:link :rel "stylesheet" :href "/static/css/content.css")
       (:link :rel "stylesheet" :href "/theme/light.css"))
     (:body
       (:header
         (:button :class "flip-button" ">")
         (:h1 "What?")
         )
       (:section :id "content"
        (:section :id "sidebar"
         (cl-markup:raw (feedlist-markup *feeds*)))
        (:main
          (loop for feed across *feeds*
                count feed into feed-count
                collect
                (feed-markup feed feed-count))))
       (:footer))))

(defun base-template-f () (base-template))

;(cl-oid-connect:def-route ("/" (params) :app *app*)
;  (ningle:with-context-variables (session)
;    (cl-oid-connect:redirect-if-necessary session
;      (cl-oid-connect:require-login
;        (anaphora:sunless (gethash :counter session) (setf anaphora:it 0))
;        (incf (gethash :counter session))
;        (format nil "~Ath visit<br/>~a<br/><br/>"
;                (gethash :counter session))))))

(cl-oid-connect:def-route ("/reflect" (params) :app *app* :method :post)
  (format nil "~s<hr/>" (jonathan.encode:to-json (jonathan:parse (car (elt params 0))))))

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

(cl-oid-connect:def-route ("/" (params) :app *app*)
  (ningle.context:with-context-variables (session)
      ;(cl-oid-connect:require-login
        (cl-oid-connect:redirect-if-necessary session
          (let ((*feeds* (gethash :feeds session *feeds*)))
          (base-template-f)));)
  ))

;;; this will be bound by calls to with-palette
;;; probably should be refactored out
(defparameter *palette* nil)

(defparameter *colorscheme* (make-instance 'colors:colorscheme))

(cl-oid-connect:def-route ("/theme/dark.css" (params) :app *app*)
  (colors:let-palette (make-instance 'colors:palette)
    (eval '(get-theme-css))))

(cl-oid-connect:def-route ("/theme/light.css" (params) :app *app*)
  (colors:let-palette (colors:invert-palette (make-instance 'colors:palette))
    (eval '(get-theme-css))))



(defun get-theme-css ()
  (colors:with-palette (*palette*)
    (flet ((combine-unit-q (quant unit) (format nil "~d~a" quant unit)))
      (let* ((header-height 9)
             (height-units "vh")
             (ss (lass:compile-and-write
                   `(* :color ,(colors:colorscheme-fg *colorscheme*))

                   `(body :background-color ,(colors:colorscheme-bg *colorscheme*))

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
                        :float right
                        :width "3em"
                        :height "3em"
                        :padding-left "1em"
                        :padding-bottom "1em"
                        :border-bottom-left-radius "100%"
                        :border none
                        :transition "all 2s cubic-bezier(0.175, 0.885, 0.32, 1.275)"
                        :background-color ,(colors:colorscheme-fg *colorscheme*)
                        :color ,(colors:colorscheme-bg *colorscheme*))
                      ((:and .flip-button :focus)
                       :outline none)
                      ((:and .flip-button :hover)
                       :width "6em"
                       :height "6em"
                       :padding-left "4em"
                       :padding-bottom "3em")
                      )

                   `(main
                      :border-left thin solid ,(colors:colorscheme-accent *colorscheme*)
                      :height ,(combine-unit-q (- 100 header-height) height-units))

                   `((:or a (:and a :visited) (:and a :active) code.url)
                     :color ,(colors:colorscheme-fg-highlight *colorscheme*))

                   `(section#sidebar
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

(defvar *handler* nil)

(defun stop ()
  (clack:stop (pop *handler*)))

(defun start (&optional tmp)
  (let ((server (if (> (length tmp) 1)
                  (intern (string-upcase (elt tmp 1)) 'keyword)
                  :hunchentoot)))
   (push (clack:clackup
          (lack.builder:builder
            :backtrace
            :session
            ;:csrf
            (:static :path "/static/" :root #p"./static/")
            (funcall
              (cl-oid-connect:oauth2-login-middleware
                :facebook-info (truename "~/github_repos/cl-oid-connect/facebook-secrets.json")
                :google-info (truename "~/github_repos/cl-oid-connect/google-secrets.json"))
              *app*)) :port 9090 :server server)
        *handler*)))



