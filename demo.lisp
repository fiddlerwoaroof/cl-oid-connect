(in-package :cl-user)
(ql:quickload :cl-oid-connect)
(ql:quickload :plump)
(ql:quickload :cl-markup)

(push (cons "application" "rdf+xml") drakma:*text-content-types*)
(push (cons "text" "rss+xml") drakma:*text-content-types*)

(defparameter *app* (make-instance 'ningle:<app>))

(defclass rss-feed ()
  ((feed :accessor rss-feed-feed
         :initarg :feed)
   (channel :accessor rss-feed-channel)
   (title :accessor rss-feed-title)
   (link :accessor rss-feed-link)
   (description :accessor rss-feed-description)
   (items :accessor rss-feed-items)))

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

(defmacro get-elements (feed &optional (filter nil))
  (let ((feed-sym (gensym))
        (filter-lis `(lambda (x) (and (plump-dom:element-p x) ,@(loop for x in filter
                                                                      collect `(funcall ,x x))))))
    `(let ((,feed-sym ,feed))
       (remove-if-not ,filter-lis (plump:children ,feed-sym)))))

(defmacro get-elements-by-tagname (feed tagname)
  `(get-elements ,feed ((lambda (x) (string= ,tagname (plump:tag-name x))))))

(defmacro extract-text (selector &optional (default ""))
  (alexandria:with-gensyms (selector-s)
    `(let ((,selector-s ,selector))
       (if (not (equalp #() (lquery:$ ,selector-s)))
         (lquery:$ ,selector-s (text) (node))
         ,default))))

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
           (description-raw (let ((plump:*html-tags*)
                                  (ss (make-string-output-stream)))
                              (plump:serialize
                                (plump:parse (extract-text "description"))
                                ss)
                              (get-output-stream-string ss)))
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
         (channel (lquery:$ "channel" (text) (node)))
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
    "http://www.reddit.com/r/roguelikedev.rss"
    "http://www.reddit.com/r/roguelikes.rss"
    "http://www.reddit.com/r/talesfromtechsupport.rss"
    "https://thomism.wordpress.com/feed/rss"
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

(defclass palette () ; soloarized http://ethanschoonover.com/solarized
  ((base03     :accessor palette-base03      :initform "#002b36")
   (base02     :accessor palette-base02      :initform "#073642")
   (base01     :accessor palette-base01      :initform "#586e75")
   (base00     :accessor palette-base00      :initform "#657b83")
   (base0      :accessor palette-base0       :initform "#839496")
   (base1      :accessor palette-base1       :initform "#93a1a1")
   (base2      :accessor palette-base2       :initform "#eee8d5")
   (base3      :accessor palette-base3       :initform "#fdf6e3")
   (yellow     :accessor palette-yellow      :initform "#b58900")
   (orange     :accessor palette-orange      :initform "#cb4b16")
   (red        :accessor palette-red         :initform "#dc322f")
   (magenta    :accessor palette-magenta     :initform "#d33682")
   (violet     :accessor palette-violet      :initform "#6c71c4")
   (blue       :accessor palette-blue        :initform "#268bd2")
   (cyan       :accessor palette-cyan        :initform "#2aa198")
   (green      :accessor palette-green       :initform "#859900")))

(defparameter *palette* (make-instance 'palette))
(defgeneric invert-palette (palette))

(defmacro initialize-to (obj1-v obj2-v &body slot-swaps)
  (alexandria:with-gensyms (obj1 obj2)
    `(let* ((,obj1 ,obj1-v)
            (,obj2 ,obj2-v))
       ,@(loop for (to from) in slot-swaps
               collect `(setf (,to ,obj1) (,from ,obj2))))))

(defmethod invert-palette ((palette palette))
  (let ((result (make-instance 'palette)))
    (initialize-to result palette
      (palette-base03 palette-base3)
      (palette-base02 palette-base2)
      (palette-base01 palette-base1)
      (palette-base00 palette-base0)
      (palette-base0  palette-base00)
      (palette-base1  palette-base01)
      (palette-base2  palette-base02)
      (palette-base3  palette-base03))
    result))
(setf *palette* (invert-palette *palette*))

(defclass colorscheme ()
  ((bg           :accessor -colorscheme-bg           :initform 'base03)
   (bg-highlight :accessor -colorscheme-bg-highlight :initform 'base02)
   (fg-deemph    :accessor -colorscheme-fg-deemph    :initform 'base01)
   (fg           :accessor -colorscheme-fg           :initform 'base0 )
   (fg-highlight :accessor -colorscheme-fg-highlight :initform 'base1 )
   (accent       :accessor -colorscheme-accent       :initform 'violet)))

(defgeneric accentize (colorscheme accent))
(defmethod accentize ((colorscheme colorscheme) accent)
  (setf (colorscheme-accent colorscheme) (funcall accent colorscheme)))

(defmacro def-palette-accessor (scheme-slot scheme palette )
  `(progn
     (defgeneric ,scheme-slot (,scheme))
     (defmethod ,scheme-slot ((,scheme colorscheme))
       (slot-value ,palette (,(intern (concatenate 'string "-" (symbol-name scheme-slot))) ,scheme)))))

(def-palette-accessor colorscheme-bg scheme *palette*)
(def-palette-accessor colorscheme-bg-highlight scheme *palette*)
(def-palette-accessor colorscheme-fg-deemph scheme *palette*)
(def-palette-accessor colorscheme-fg scheme *palette*)
(def-palette-accessor colorscheme-fg-highlight scheme *palette*)
(def-palette-accessor colorscheme-accent scheme *palette*)

(defgeneric rebase (colorscheme))
(defmethod rebase ((colorscheme colorscheme))
  (macrolet
    ((swap-color (obj slot color1 color2)
       `(setf (,slot ,obj)
             (if (string= (,slot ,obj) (,color1 ,obj))
               (,color2 ,obj)
               (,color1 ,obj)))))
    ; Note that swap-color doesn't use gensyms: so don't run functions in invocation
    (swap-color colorscheme colorscheme-accent colorscheme-base1 colorscheme-base01)
    (swap-color colorscheme colorscheme-bg colorscheme-base3 colorscheme-base03)
    (swap-color colorscheme colorscheme-bg-highlight colorscheme-base3 colorscheme-base03)
    (swap-color colorscheme colorscheme-deemph colorscheme-base0 colorscheme-base0)
    (swap-color colorscheme colorscheme-fg colorscheme-base0 colorscheme-base0)
    (swap-color colorscheme colorscheme-fg-highlight colorscheme-base0 colorscheme-base0)
    colorscheme))


(defparameter *colorscheme* (make-instance 'colorscheme))
(rebase *colorscheme*)
(accentize *colorscheme* #'colorscheme-blue)

;rebase  $base3, $base2, $base1, $base0,$base00,$base01,$base02,$base03
;rebase $base03,$base02,$base01,$base00 ,$base0 ,$base1 ,$base2 ,$base3



(cl-oid-connect:def-route ("/theme/dark.css" (params) :app *app*)
  (let ((*palette* (make-instance 'palette)))
    (eval '(get-theme-css))))

(cl-oid-connect:def-route ("/theme/light.css" (params) :app *app*)
  (let ((*palette* (invert-palette (make-instance 'palette))))
    (eval '(get-theme-css))))

(defun get-theme-css ()
  (flet ((combine-unit-q (quant unit) (format nil "~d~a" quant unit)))
    (let* ((header-height 19)
           (height-units "vh")
           (ss (lass:compile-and-write
                 `(* :color ,(colorscheme-fg *colorscheme*))

                 `(body :background-color ,(colorscheme-bg *colorscheme*))

                 `((:or h1 h2 h3)
                   :color ,(colorscheme-fg-highlight *colorscheme*))
                 `(.feed-header
                    :background-color ,(colorscheme-bg-highlight *colorscheme*))

                 `((:or h4 h5 h6) :color ,(colorscheme-fg-highlight *colorscheme*))

                 `(header
                    :border-bottom "thin" "solid" ,(colorscheme-accent *colorscheme*)
                    :height ,(combine-unit-q header-height height-units)
                    :font-size ,(combine-unit-q (* 0.75 header-height) height-units)
                    :line-height ,(combine-unit-q header-height height-units)
                    (.flip-button
                      :float right))

                 `(main
                    :border-left thin solid ,(colorscheme-accent *colorscheme*)
                    :height ,(combine-unit-q (- 100 header-height) height-units))

                 `((:or a (:and a :visited) (:and a :active) code.url)
                   :color ,(colorscheme-fg-highlight *colorscheme*))

                 `(section#sidebar
                    (ul.menu
                      ((li + li)
                       :border-top "thin" "solid" ,(colorscheme-fg-highlight *colorscheme*))
                      ((:and li :hover)
                       :background-color ,(colorscheme-bg-highlight *colorscheme*)
                       :color ,(colorscheme-fg-highlight *colorscheme*))))

                 `(.feed
                    :border-bottom thin solid ,(colorscheme-fg *colorscheme*)
                    :border-left none)

                 `(.link
                    :border-top thin solid ,(colorscheme-fg *colorscheme*)
                    :border-bottom none

                    (.link-header :background-color ,(colorscheme-bg-highlight *colorscheme*))

                    (.link-info
                      :color ,(colorscheme-fg-deemph *colorscheme*)
                      :border-bottom "thin" "solid" ,(colorscheme-fg *colorscheme*)
                      ((:or a span)
                       :color inherit)
                      ((:and a :hover)
                       :color ,(colorscheme-fg *colorscheme*))
                      ))
                 `((:and .feed-header :hover)
                   :background-color ,(colorscheme-bg *colorscheme*))
                 `(.link.closed
                    (.link-header
                      :background-color ,(colorscheme-bg *colorscheme*))
                    ((:and .link-header :hover)
                     :background-color ,(colorscheme-bg-highlight *colorscheme*)))

                 )))
      `(200 (:content-type "text/css") ,ss))))

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
         (:button :class "flip-button" "c") 
         (:h1 "Feeds")
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
    (cl-oid-connect:require-login
      (cl-oid-connect:require-login
        (let ((*feeds* (gethash :feeds session *feeds*)))
          (base-template-f))))))

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



