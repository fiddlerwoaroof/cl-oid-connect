(in-package :cl-user)
(use-package :lquery)
(declaim (optimize (safety 3) (speed 0) (debug 3)))

(define-lquery-list-function where-attr (els attr &key is)
  (remove-if-not (lambda (y) (string= (plump:attribute y attr) is))
                 els))

(define-lquery-list-function tag-name (nodes &rest tags)
  "Manipulate elements on the basis of their tag-name.  With no arguments,
   return their names else return the corresponding tags."
  (if (null tags)
    (map 'vector #'plump:tag-name nodes)
    (apply #'vector
           (loop for node across nodes
                 if (find (plump:tag-name node) tags :test #'string=)
                 collect node))))



(load "tables.lisp")

(defpackage :whitespace.feeds.autodiscovery
  (:use #:cl #:alexandria #:postmodern #:lquery #:cl-syntax #:cl-annot.syntax #:cl-annot.class
        #:whitespace.tables #:iterate #:whitespace.utils)
  (:import-from anaphora it))
(in-package #:whitespace.feeds.autodiscovery)

(defun discover-feeds (doc)
  "Given a plump DOM element, discover any feeds in the document using the link tags.
   Returns a vector "
  (coerce
    (lquery:$ (inline doc)
            "link"
            (where-attr "rel" :is "alternate")
            (combine (attr "href")
                     (attr "type")))
    'list))
(export 'discover-feeds)

(defpackage :whitespace.feeds.opml
  (:use #:cl #:alexandria #:postmodern #:lquery #:cl-syntax #:cl-annot.syntax #:cl-annot.class
        #:whitespace.tables #:iterate #:whitespace.utils)
  (:import-from anaphora it))
(in-package #:whitespace.feeds.opml)



(in-package :cl-user)
(defpackage :whitespace.feeds.rss
  (:use #:cl #:alexandria #:postmodern #:lquery #:cl-syntax #:cl-annot.syntax #:cl-annot.class
        #:whitespace.tables #:iterate #:whitespace.utils #:whitespace.feeds.autodiscovery)
  (:import-from anaphora it))


(in-package :whitespace.feeds.rss)
(cl-annot.syntax:enable-annot-syntax)

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
(defmacro xml-text-bind (syms &body body)
  "Bind the symbols passed in the second arg to the text of the matching
   elements in the document lquery has been initialized with and then run the
   body in the resulting lexical scope.  This assumes that lquery:initialize
   has already been passed the proper xml document"
  `(let* ,(loop for sym in syms
           collect `(,sym (or (lquery:$ ,(symbol-name sym) (text) (node)) "")))
     ,@body))

(defmacro make-instance-from-symbols (class &rest initargs)
  `(make-instance ,class ,@(iterate (for (to from) in (ensure-mapping initargs))
                                    (appending (list (make-keyword (symbol-name to)) from)))))

@export-class
(defclass rss-feed ()
  ((feed :accessor rss-feed-feed :initarg :feed)
   (channel :accessor rss-feed-channel :initarg :channel)
   (title :accessor rss-feed-title :initarg :title)
   (link :accessor rss-feed-link :initarg :link)
   (description :accessor rss-feed-description :initarg :description)
   (items :accessor rss-feed-items :initarg :items)
   (fetch-url :accessor fetch-url :initarg :fetch-url)))

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

@export
(defun make-rss-item (item fallback-date)
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
                                (extract-text "description")))))
           ;(enclosure) --- TODO: implement comment / enclosure handling

      (xml-text-bind (title link guid pubdate source comments)
        (when (string= pubdate "")
          (setf pubdate fallback-date))
        (make-instance-from-symbols 'rss-item
                                    item title link
                                    description-raw
                                    (description (dehtml description-raw))
                                    (category (get-category-names (lquery:$ "category")))
                                    guid (pub-date pubdate) source comments)))))
      ;(setf (rss-item-enclosure result) enclosure)      -- TODO: comment/enclosure . . .


@export
(defun make-rss-feed (feed)
  (lquery:initialize feed)
  (let* ((channel (lquery:$ "channel" (node)))
         (fetch-url (lquery:$ "channel" (children) (tag-name "atom:link") (filter "[rel=self]") (attr :href) (node)))
         (link (lquery:$ "channel > link" (text) (node)))
         (link (if (string= link "") (lquery:$ "channel" (children) (tag-name "atom:link") (attr :href) (node)) link))
         (items (lquery:$ "item"))
         (last-build (or (lquery:$ "lastBuildDate" (text) (node)) ""))
         (pub-date (default-when last-build (string= last-build "")
                     (lquery:$ "pubDate" (text) (node))))
         (fallback-date (if (string= pub-date "") "2015-01-01 0:0:0+00" pub-date)))
    (format t "fallback-date: ~a~%" fallback-date)
    (xml-text-bind (title description)
      (make-instance-from-symbols 'rss-feed
                                  feed title link description channel fetch-url
                                  (items (iterate (for it in-sequence items)
                                                  (collecting (make-rss-item it fallback-date))))))))

; These are the interface I'm planning to remove as duplicate
(defserializer (rss-feed)
  title link description fetch-url
  (items (iterate (for item in items)
                  (collect item))))

(defserializer (rss-item)
  title link (description description-raw :bind-from description-raw) guid pub-date source)

(defmethod jonathan:%to-json ((obj rss-feed))
  (jonathan:%to-json (serialize obj #'alexandria:alist-hash-table #'%json-pair-transform)))

(defmethod jonathan:%to-json ((obj rss-item))
  (jonathan:%to-json (serialize obj #'alexandria:alist-hash-table #'%json-pair-transform)))

(defmacro get-id-for-object ((table key-column &optional (id-column :id)) key &body body)
  "Anaphoric macro: binds id to the id it retrieves!"
  (once-only (id-column key)
    `(let ((id (anaphora:awhen (postmodern:query (:select ,id-column :from ',table :where (:= ',key-column ,key)))
                 (caar it))))
       ,@body)))

; NOTE: this won't make dao objects for the _items_ when called on the feed!
; also NOTE: this _prefers_ the passed object
(defmethod get-dao-for ((obj rss-feed) &optional linked-objects)
 (declare (ignore linked-objects))
 (with-slots (title link description fetch-url) obj
   (get-id-for-object (rss_feed_store link) link
     (make-instance-from-symbols 'rss_feed_store id title link description fetch-url (fetch-defaults t))) ))

(defmethod get-dao-for ((obj rss-item) &optional feed)
 (with-slots (title link description-raw guid pub-date source) obj
   (get-id-for-object (rss_item_store guid) guid
     (let ((result (make-instance-from-symbols 'rss_item_store title link (description description-raw)
                                 guid pub-date source feed (fetch-defaults t))))
       (unless (null id)
         (setf (ris-id result) id))
       result))))

(defun get-and-possibly-store-feed (rss-feed)
  "Given an rss-feed, return the db's feed-id, persisting it if it doesn't already exist."
  (postmodern:ensure-transaction
    (anaphora:aif (postmodern:select-dao 'rss_feed_store (:= 'link (rss-feed-link rss-feed)))
      (car anaphora:it) ;; The postmodern query returns a nested list
      (store-feed-dao (serialize rss-feed)))))

@export
(defun store-feed (doc)
  (postmodern:with-transaction ()
    (let ((rss-feed (make-rss-feed doc)))
      (values rss-feed
              (get-and-possibly-store-feed rss-feed)))))

@export ; TODO: this should eventually take a username/userobject rather than ids . . .
(defun subscribe-to-feed (uid feedid)
  (postmodern:query
    (:insert-into 'subscriptions :set 'uid uid 'feedid feedid)))

#|
(:documentation
  "Store a serialized rss object into rhe database: the basic idea here is
   that the quasi-quoted expression generates a form that would insert the
   item and then we eval it.")
|#

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
                                     (:order-by
                                       (:select :* :from 'rss_item_store
                                        :where (:= :feed feed-id))
                                       (:desc 'pub-date)))))
    (loop for item in items collect (deserialize-item item))))

@export
(defun deserialize-feed (feed)
  (let ((result (make-instance 'rss-feed)))
    (copy-slots (title link description fetch-url) feed result)
    (setf (rss-feed-items result) (deserialize-items (rfs-id feed)))
    result))

@export
(defun deserialize (&optional user-info)
  (default-when #() (not (null user-info))
    (let ((feeds
            (postmodern:query-dao 'rss_feed_store
                                  (:select 'rss_feed_store.*
                                   :from 'rss_feed_store
                                   :inner-join  'subscriptions :on (:= 'rss_feed_store.id  'subscriptions.feedid)
                                   :inner-join  'reader_user :on (:= 'reader_user.id  'subscriptions.uid)
                                   :where (:= 'reader_user.foreign_id (user-foreign-id user-info))))))
      (apply #'vector (loop for feed in feeds collect (deserialize-feed feed))))))

(export
  (defun get-feed-from-dao (rss-feed)
    (let ((feed-dao (get-dao-for rss-feed)))
      (list feed-dao
            (with-slots (items) rss-feed
              (iterate (for item in items)
                       (collect (get-dao-for item (slot-value feed-dao 'id)))))))))


@export
(defun upsert-feed (rss-feed)
  (postmodern:ensure-transaction
    (destructuring-bind (feed items) (get-feed-from-dao rss-feed)
      (postmodern:upsert-dao feed)
      (mapcar #'postmodern:upsert-dao items))))


; \o/
;  | Arrr
; / \
