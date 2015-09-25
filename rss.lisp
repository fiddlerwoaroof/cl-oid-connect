(in-package :cl-user)

(load "tables.lisp")

(defpackage :whitespace.rss
  (:use #:cl #:alexandria #:postmodern #:lquery #:cl-syntax #:cl-annot.syntax #:cl-annot.class
        #:whitespace.tables #:iterate)
  (:import-from anaphora it))


(in-package :whitespace.rss)
(cl-annot.syntax:enable-annot-syntax)

(defun ensure-mapping (list)
  "Make sure that each item of the list is a pair of symbols"
  (mapcar (lambda (x) (if (symbolp x) (list x x) x)) list))

(defun alist-string-hash-table (alist)
  (alexandria:alist-hash-table alist :test #'string=))

(defun transform-alist (pair-transform alist)
  (iterate (for (k . v) in-sequence alist)
           (collect
             (funcall pair-transform k v))))

(defun %json-pair-transform (k v)
  (cons (make-keyword (string-downcase k))
        (typecase v
          (string (coerce v 'simple-string))
          (t v))))

(defun %default-pair-transform (k v)
  (cons (make-keyword (string-upcase k)) v))

(defun make-pairs (symbols)
  (cons 'list (iterate (for (key value) in symbols)
                       (collect (list 'list* (symbol-name key) value)))))

@export
(defmacro copy-slots (slots from-v to-v)
  (with-gensyms (from to)
    `(let ((,from ,from-v) (,to ,to-v))
       ,@(iterate (for (fro-slot to-slot) in (ensure-mapping slots))
                  (collect `(setf (slot-value ,to ',to-slot) (slot-value ,from ',fro-slot))))
       ,to)))

@export
(defmacro default-when (default test &body body)
  (once-only (default)
    `(or (when ,test
           ,@body)
         ,default)))

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

(defmacro transform-result ((list-transform pair-transform) &body alist)
    `(funcall ,list-transform
              (transform-alist ,pair-transform
                               ,@alist)))


(defmacro slots-to-pairs (obj (&rest slots))
  (alexandria:once-only (obj)
    (let ((slots (ensure-mapping slots)))
      `(with-slots ,(mapcar #'cadr slots) ,obj
         ,(make-pairs slots)))))

(defmacro defserializer ((specializes) &body slots)
  (with-gensyms (obj o-t p-t)
    `(defmethod serialize ((,obj ,specializes) &optional (,o-t #'identity) (,p-t #'%default-pair-transform))
      (transform-result (,o-t ,p-t)
        (slots-to-pairs ,obj ,slots)))))

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
(defun make-rss-feed (feed)
  (lquery:initialize feed)
  (let* ((channel (lquery:$ "channel" (node)))
         (fetch-url (lquery:$ "channel" (children) (tag-name "atom:link") (filter "[rel=self]") (attr :href) (node)))
         (link (lquery:$ "channel > link" (text) (node)))
         (link (if (string= link "") (lquery:$ "channel" (children) (tag-name "atom:link") (attr :href) (node)) link))
         (items (lquery:$ "item")))
    (xml-text-bind (title description)
      (make-instance-from-symbols 'rss-feed
                                  feed title link description channel fetch-url
                                  (items (iterate (for it in-sequence items)
                                                  (collecting (make-rss-item it))))))))

@export
(defgeneric serialize (cls &optional output-transform pair-transform))


(defmethod serialize ((obj sequence) &optional (o-t #'identity) (p-t #'%default-pair-transform))
  (iterate (for item in-sequence obj)
           (collect (serialize item o-t p-t))))

; These are the interface I'm planning to remove as duplicate
(defserializer (rss-feed)
  title link description fetch-url
  (items (iterate (for item in items)
                  (collect item))))

(defserializer (rss-item)
  title link (description description-raw) guid pub-date source)

; this is the interface to be used
(defserializer (rss_feed_store)
  title link description fetch-url)

(defserializer (rss_item_store)
  title link description fetch-url)

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

(defgeneric get-dao-for (obj &optional link)
  ; NOTE: this won't make dao objects for the _items_ when called on the feed!
  ; also NOTE: this _prefers_ the passed object
  (:method ((obj rss-feed) &optional linked-objects)
   (declare (ignore linked-objects))
   (with-slots (title link description fetch-url) obj
     (get-id-for-object (rss_feed_store link) link
       (make-instance-from-symbols 'rss_feed_store id title link description fetch-url (fetch-defaults t))) ))

  (:method ((obj rss-item) &optional feed)
   (with-slots (title link description-raw guid pub-date source) obj
     (get-id-for-object (rss_item_store guid) guid
       (make-instance-from-symbols 'rss_item_store id title link (description description-raw)
                                   guid pub-date source feed (fetch-defaults t))))))

@export
(defun get-feed-from-dao (rss-feed)
  (let ((feed-dao (get-dao-for rss-feed)))
    (list feed-dao
          (with-slots (items) rss-feed
            (iterate (for item in items)
                     (collect (get-dao-for item (slot-value feed-dao 'id))))))))


@export
(defun upsert-feed (rss-feed)
  (postmodern:ensure-transaction
    (destructuring-bind (feed items) (get-feed-from-dao rss-feed)
      (postmodern:upsert-dao feed)
      (mapcar #'postmodern:upsert-dao items))))

; TODO: get rid of eval
@export
(defun store-feed-dao (serialized-rss-feed &optional link)
  (declare (ignore link))
  (let* ((items nil)
         (rss_feed (apply #'postmodern:make-dao
                          (cons 'rss_feed_store
                                (iterate (for (k . v) in-sequence serialized-rss-feed)
                                         (if (eql k :items)
                                           (setf items v)
                                           (appending (list k v))))))))
    (iterate (for item in items)
             (store-item-dao (serialize item)
                             (slot-value rss_feed 'id)))
    rss_feed))

@export
(defun store-item-dao (serialized-rss-item link)
 (eval `(postmodern:make-dao
          'rss_item_store
          :feed ,link
          ,@(iterate (for (k . v) in-sequence serialized-rss-item)
                     (appending (list k v))))))

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
                                     (:select :* :from 'rss_item_store :where (:= :feed feed-id)))))
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


@export
(defun normalize-html (html)
  (let ((plump-parser:*tag-dispatchers* plump:*html-tags*))
    (with-output-to-string (ss)
      (map 'vector
           (lambda (x) (plump:serialize (plump:parse (plump:text x)) ss))
           html)
      ss)))

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
        (make-instance-from-symbols 'rss-item
                                    item title link description-raw (description description-munged)
                                    category guid pub-date source comments)))))
      ;(setf (rss-item-enclosure result) enclosure)      -- TODO: comment/enclosure . . .


; \o/
;  | Arrr
; / \
