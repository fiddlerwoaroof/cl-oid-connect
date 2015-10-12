(in-package :cl-user)
(defpackage :whitespace.tables
  (:use #:cl #:alexandria #:postmodern #:annot.class #:iterate #:whitespace.utils))
(in-package :whitespace.tables)
(cl-annot.syntax:enable-annot-syntax)

@export-class
(defclass rss_feed_store ()
  ((id          :col-type serial :initarg :id          :accessor rfs-id)
   (title       :col-type text   :initarg :title       :accessor rfs-title       :col-default "")
   (link        :col-type text   :initarg :link        :accessor rfs-link        :col-default "")
   (description :col-type text   :initarg :description :accessor rfs-description :col-default "")
   (fetch-url   :col-type text   :initarg :fetch-url :accessor rfs-fetch-url   :col-default ""))
  (:metaclass dao-class)
  (:keys id))

(deftable rss_feed_store
  (!dao-def)
  (!unique "link"))


@export-class
(defclass rss_item_store ()
  ((id          :col-type serial  :initarg :id          :accessor ris-id)
   (title       :col-type text    :initarg :title       :accessor ris-title       :col-default "")
   (link        :col-type text    :initarg :link        :accessor ris-link        :col-default "")
   (description :col-type text    :initarg :description :accessor ris-description :col-default "")
   (comments    :col-type text    :initarg :comments    :accessor ris-comments    :col-default "")
   (enclosure   :col-type text    :initarg :enclosure   :accessor ris-enclosure   :col-default "")
   (guid        :col-type text    :initarg :guid        :accessor ris-guid        :col-default "")
   (pub-date    :col-type text    :initarg :pub-date    :accessor ris-pub-date    :col-default "")
   (source      :col-type text    :initarg :source      :accessor ris-source      :col-default "")
   (feed        :col-type integer :initarg :feed        :accessor ris-feed))
  (:metaclass dao-class)
  (:keys id))

(deftable rss_item_store
  (!dao-def)
  (!foreign "rss_feed_store" "feed" "id" :on-delete :cascade :on-update :cascade)
  (!unique "guid"))


@export-class
(defclass reader_user ()
  ((id :col-type serial)
   (join-date :col-type timestamp :accessor join-date :col-default (:now))
   (foreign-id :col-type string :initarg :foreign-id :accessor user-foreign-id :unique t)
   (name :col-type string :initarg :name :accessor user-name)
   (email :col-type string :initarg :email :accessor user-email)
   (first-name :col-type (or string s-sql:db-null) :initarg :first-name :accessor user-first-name)
   (gender :col-type (or string s-sql:db-null) :initarg :gender :accessor user-gender)
   (last-name :col-type (or string s-sql:db-null) :initarg :last-name :accessor user-last-name)
   (link :col-type (or string s-sql:db-null) :initarg :link :accessor user-link)
   (locale :col-type (or string s-sql:db-null) :initarg :locale :accessor user-locale))
  (:metaclass dao-class)
  (:keys id))

(deftable reader_user
  (!dao-def))

@export-class
(defclass subscriptions ()
  ((id :col-type serial)
   (uid :col-type integer :initarg :uid :accessor subscription-uid)
   (feedid :col-type integer :initarg :feedid :accessor subscription-feedid))
  (:unique (uid feedid))
  (!foreign "rss_feed_store" "feedid" "id" :on-delete :cascade :on-update :cascade)
  (:metaclass dao-class)
  (:keys id))

(deftable subscriptions
  (!dao-def)
  (!foreign "rss_feed_store" "feedid" "id" :on-delete :cascade :on-update :cascade)
  (!foreign "reader_user" "uid" "id" :on-delete :cascade :on-update :cascade)
  (!unique '(uid feedid)))


@export
(defgeneric get-dao-for (obj &optional link)
  (:documentation "Take an object an return the equivalent dao object. Use link to specify a single
                   foreign key relationship---this probably should be generalized further"))

@export
(defgeneric serialize (cls &optional output-transform pair-transform))

@export
(defmacro defserializer ((specializes) &body slots)
  (with-gensyms (obj o-t p-t)
    `(defmethod serialize ((,obj ,specializes) &optional (,o-t #'identity) (,p-t #'%default-pair-transform))
      (transform-result (,o-t ,p-t)
        (slots-to-pairs ,obj ,slots)))))

(defmethod serialize ((obj sequence) &optional (o-t #'identity) (p-t #'%default-pair-transform))
  (iterate (for item in-sequence obj)
           (collect (serialize item o-t p-t))))

; this is the interface to be used
(defserializer (rss_feed_store)
  title link description fetch-url)

(defserializer (rss_item_store)
  title link description fetch-url)

@export
(defun store-item-dao (serialized-rss-item link)
  (pomo:ensure-transaction
    (apply #'postmodern:make-dao
           (list* 'rss_item_store :feed link
                  (iterate (for (k . v) in-sequence serialized-rss-item)
                           (appending (list k v)))))))

@export
(defun store-feed-dao (serialized-rss-feed &optional link)
  (declare (ignore link))
  (pomo:ensure-transaction
    (let* ((items nil)
           (rss_feed (apply #'postmodern:make-dao
                            (cons 'rss_feed_store
                                  (iterate (for (k . v) in-sequence serialized-rss-feed)
                                           (if (eql k :items)
                                             (setf items v)
                                             (appending (list k v))))))))
      (iterate (for item in items)
               (handler-case (pomo:with-savepoint store-item
                               (store-item-dao (serialize item) (slot-value rss_feed 'id)))
                 (cl-postgres-error:unique-violation ())))
      rss_feed)))


#|

(with-connection whitespace::*db-connection-info* 
  (with-transaction ()
    (create-table 'rss_feed_store)
    (create-table 'rss_item_store)
    (create-table 'reader_user)
    (create-table 'subscriptions)))

|#
