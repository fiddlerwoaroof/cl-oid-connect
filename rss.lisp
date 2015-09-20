;(in-package :cl-user)
;(defpackage :whitespace.rss
;  (:shadow "to-json")
;  (:use #:cl #:alexandria #:postmodern #:lquery #:cl-syntax #:cl-annot.syntax #:cl-annot.class
;        #:whitespace.tables #:iterate))

(in-package :whitespace.rss)
(cl-annot.syntax:enable-annot-syntax)


@export
(defmacro default-when (default test &body body)
  (once-only (default)
    `(or (when ,test
           ,@body)
         ,default)))

@export-class
(defclass rss-feed ()
  ((feed :accessor rss-feed-feed :initarg :feed)
   (channel :accessor rss-feed-channel :initarg :channel)
   (title :accessor rss-feed-title :initarg :title)
   (link :accessor rss-feed-link :initarg :link)
   (description :accessor rss-feed-description :initarg :description)
   (items :accessor rss-feed-items :initarg :items)))

@export-class
(defclass rss-item ()
  ((item :accessor rss-item-item  :initarg :item)
   (title :accessor rss-item-title :initarg :title)
   (link :accessor rss-item-link :initarg :link)
   (description-raw :accessor rss-item-description-raw :initarg :description-raw)
   (description :accessor rss-item-description :initarg :description)
   (category :accessor rss-item-category :initarg :category)
   (comments :accessor rss-item-comments :initarg :comments)
   (enclosure :accessor rss-item-enclosure :initarg :enclosure)
   (guid :accessor rss-item-guid :initarg :guid)
   (pub-date :accessor rss-item-pub-date :initarg :pub-date)
   (source :accessor rss-item-source  :initarg :source)))

(load "tables.lisp")

(defmethod jonathan:%to-json ((obj rss-feed)) (jonathan:%to-json (serialize obj
                                                                            #'alexandria:alist-hash-table
                                                                            #'%json-pair-transform)))
(defmethod jonathan:%to-json ((obj rss-item)) (jonathan:%to-json (serialize obj
                                                                            #'alexandria:alist-hash-table
                                                                            #'%json-pair-transform)))

(defun alist-string-hash-table (alist)
  (alexandria:alist-hash-table alist :test #'string=))

(defun transform-alist (pair-transform alist)
  (iterate (for (k . v) in-sequence alist)
           (collect
             (funcall pair-transform k v))))

(defun %json-pair-transform (k v)
  (cons (make-keyword k)
        (typecase v
          (string (coerce v 'simple-string))
          (t v))))

(defun %default-pair-transform (k v)
  (cons (make-keyword (string-upcase k)) v))

@export
(defgeneric serialize (cls &optional output-transform pair-transform)
  (:method ((obj sequence) &optional (output-transform #'identity) (pair-transform #'%default-pair-transform))
   (iterate (for item in-sequence obj)
            (collect (serialize item output-transform pair-transform))))

  (:method ((obj rss-feed) &optional (output-transform #'identity) (pair-transform #'%default-pair-transform))
   (funcall output-transform
     (transform-alist pair-transform
                      `(("title" . ,(rss-feed-title obj))
                        ("link" . ,(rss-feed-link obj))
                        ("description" . ,(rss-feed-description obj))
                        ("items" . ,(iterate (for item in-sequence (rss-feed-items obj))
                                           (collect (serialize item output-transform pair-transform))))))))

  (:method ((obj rss-item) &optional (output-transform #'identity) (pair-transform #'%default-pair-transform))
   (funcall output-transform
     (transform-alist pair-transform
                      `(("title" . ,(rss-item-title obj))
                        ("link" . ,(rss-item-link obj))
                        ("description" . ,(rss-item-description-raw obj))
                        ("guid" . ,(rss-item-guid obj))
                        ("pub-date" . ,(rss-item-pub-date obj))
                        ("source" . ,(rss-item-source obj)))))))

@export
(defun store-feed-dao (serialized-rss-feed &optional link)
  (declare (ignore link))
  (let* ((items nil)
         (rss_feed (eval `(postmodern:make-dao
                            'rss_feed_store
                            ,@(iterate (for (k . v) in-sequence serialized-rss-feed)
                                       (if (eql k :items)
                                         (setf items v)
                                         (appending (list k v))))))))
    (iterate (for item in items)
             (store-item-dao item (slot-value rss_feed 'id)))
    rss_feed))

@export
(defun store-item-dao (rss-item link)
 (eval `(postmodern:make-dao
          'rss_item_store
          :feed ,link
          ,@(iterate (for (k . v) in-sequence rss-item)
                     (appending (list k v))))))

#|
(:documentation
  "Store a serialized rss object into rhe database: the basic idea here is
   that the quasi-quoted expression generates a form that would insert the
   item and then we eval it.")
|#

@export
(defmacro copy-slots (slots from-v to-v)
   (with-gensyms (from to)
     `(let ((,from ,from-v) (,to ,to-v))
        ,@(loop for (fro-slot to-slot)
                in (mapcar (lambda (x) (if (symbolp x) (list x x) x)) slots)
                collect `(setf (slot-value ,to ',to-slot) (slot-value ,from ',fro-slot))))))

@export
(defun deserialize-item (item)
  (let ((result (make-instance 'rss-item)))
    (copy-slots (title link (description description-raw) comments enclosure guid pub-date source)
                item
                result)
    result))

@export
(defun deserialize-items (feed-id)
  (let ((items (postmodern:query-dao 'rss_item_store
                                     (:select :* :from 'rss_item_store :where (:= :feed feed-id)))))
    (loop for item in items collect (deserialize-item item))))

@export
(defun deserialize-feed (feed)
  (let ((result (make-instance 'rss-feed)))
    (copy-slots (title link description) feed result)
    (setf (rss-feed-items result) (deserialize-items (rfs-id feed)))
    result))

@export
(defun deserialize (&optional user-info)
  (default-when #() (not (emptyp user-info))
    (let ((feeds
            (postmodern:query-dao 'rss_feed_store
                                  (:select 'rssfeed.*
                                   :from 'rssfeed
                                   :inner-join  'subscriptions :on (:= 'rssfeed.id  'subscriptions.feedid)
                                   :inner-join  'reader_user :on (:= 'reader_user.id  'subscriptions.uid)
                                   :where (:= 'reader_user.foreign_id (user-foreign-id (car user-info)))))))
      (apply #'vector (loop for feed in feeds collect (deserialize-feed feed))))))


@export
(defmacro get-elements (feed &optional (filter nil))
  (let ((feed-sym (gensym))
        (filter-lis `(lambda (x) (and (plump-dom:element-p x) ,@(loop for x in filter
                                                                      collect `(funcall ,x x))))))
    `(let ((,feed-sym ,feed))
       (remove-if-not ,filter-lis (plump:children ,feed-sym)))))

@export
(defmacro get-elements-by-tagname (feed tagname)
  `(get-elements ,feed ((lambda (x) (string= ,tagname (plump:tag-name x))))))

@export
(defmacro extract-text (selector &optional (default ""))
  `(or (lquery:$ ,selector (text) (node)) ,default))

@export
(defun normalize-html (html)
  (let ((plump-parser:*tag-dispatchers* plump:*html-tags*))
    (with-output-to-string (ss)
      (map 'vector
           (lambda (x) (plump:serialize (plump:parse (plump:text x)) ss))
           html)
      ss)))

@export
(defmacro xml-text-bind (syms &body body)
  "Bind the symbols passed in the second arg to the text of the matching
   elements in the document lquery has been initialized with and then run the
   body in the resulting lexical scope.  This assumes that lquery:initialize
   has already been passed the proper xml document"
  `(let* ,(loop for sym in syms
           collect `(,sym (or (lquery:$ ,(symbol-name sym) (text) (node)) "")))
     ,@body))

@export
(defun make-rss-item (item)
  (lquery:initialize item)
  (flet ((dehtml (h) (plump:text (plump:parse h)))
         (get-category-names (it) ;;; TODO: simplify this---Ask Shinmera on IRC
           (if (not (equalp #() it))
             (map 'vector
                  (lambda (x) (plump:text (elt (plump:children x) 0)))
                  it)
             #())))
    (let* ((content-encoded (lquery:$ (children) (tag-name "content:encoded")))

           (description-element (default-when content-encoded (emptyp content-encoded)
                                  (lquery:$ (children "description"))))

           (description-raw (normalize-html
                              (default-when description-element (emptyp description-element)
                                (extract-text "description"))))

           (description-munged (dehtml description-raw))
           (category (get-category-names (lquery:$ "category"))))
           ;(enclosure) --- TODO: implement comment / enclosure handling

      (xml-text-bind (title link guid pub-date source comments)
        (make-instance 'rss-item :item item
                       :title title :link link :description-raw description-raw :description description-munged
                       :category category :guid guid :pub-date pub-date :source source :comments comments)))))
      ;(setf (rss-item-enclosure result) enclosure)      -- TODO: comment/enclosure . . .

@export
(defun make-rss-feed (feed)
  (lquery:initialize feed)
  (let* ((channel (lquery:$ "channel" (node)))
         (link (extract-text "link"))
         (link (if (string= link "") (lquery:$ "channel" (children) (tag-name "atom:link") ()) link))
         (items (lquery:$ "item")))
    (xml-text-bind (title description)
      (make-instance 'rss-feed :feed feed
                     :title title :link link :description description
                     :channel channel :items (loop for it across items
                                                   collect (make-rss-item it))))))
@export
(defun store-feed (doc uid)
  (postmodern:with-transaction ()
    (let* ((rss-feed- (make-rss-feed doc))
           (feedid (anaphora:aif (postmodern:query (:select 'id :from 'rssfeed
                                                    :where (:= 'link (rss-feed-link rss-feed-))))
                     (caar anaphora:it) ;; The postmodern query returns a nested list
                     (slot-value (store-feed-dao (serialize rss-feed-)) 'id))))
      (postmodern:query
        (:insert-into 'subscriptions :set 'uid uid 'feedid feedid))
      rss-feed-)))
