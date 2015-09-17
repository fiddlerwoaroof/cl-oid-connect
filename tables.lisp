(in-package :whitespace.tables)
(cl-annot.syntax:enable-annot-syntax)

@export-class
(defclass rss_feed_store ()
  ((id          :col-type serial :initarg :id          :accessor rfs-id)
   (title       :col-type text   :initarg :title       :accessor rfs-title       :col-default "")
   (link        :col-type text   :initarg :link        :accessor rfs-link        :col-default "")
   (description :col-type text   :initarg :description :accessor rfs-description :col-default ""))
  (:metaclass postmodern:dao-class)
  (:table-name "rssFeed")
  (:unique link)
  (:keys id))

(postmodern:deftable rss_feed_store
  (postmodern:!dao-def)
  (postmodern:!unique "link")
  )


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
  (:metaclass postmodern:dao-class)
  (:keys id))

(postmodern:deftable rss_item_store
  (postmodern:!dao-def)
  (postmodern:!foreign "rssfeed" "feed" "id" :on-delete :cascade :on-update :cascade))


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
  (:metaclass postmodern:dao-class)
  (:keys id))

(postmodern:deftable reader_user
  (postmodern:!dao-def))

@export-class
(defclass subscriptions ()
  ((id :col-type serial)
   (uid :col-type integer :initarg :uid :accessor subscription-uid)
   (feedid :col-type integer :initarg :feedid :accessor subscription-feedid))
  (:unique (uid feedid))
  (:metaclass postmodern:dao-class)
  (:keys id))

(postmodern:deftable subscriptions
  (postmodern:!dao-def)
  (postmodern:!unique '(uid feedid)))

; (postmodern:create-table 'rss_feed_store)
; (postmodern:create-table 'rss_item_store)
; (postmodern:create-table 'reader_user)
; (postmodern:create-table 'subscriptions)


