(ql:quickload :postmodern)

(defclass rss-feed-store ()
  ((id          :col-type serial :initarg :id          :accessor rfs-id)
   (title       :col-type text   :initarg :title       :accessor rss-feed-title       :col-default "")
   (link        :col-type text   :initarg :link        :accessor rss-feed-line        :col-default "")
   (description :col-type text   :initarg :description :accessor rss-feed-description :col-default ""))
  (:metaclass postmodern:dao-class)
  (:table-name "rssFeed")
  (:unique link)
  (:keys id))

(postmodern:deftable rss-feed-store
  (postmodern:!dao-def))

; (postmodern:create-table 'rss-feed-store)

(defclass rss-item-store ()
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

(postmodern:deftable rss-item-store
  (postmodern:!dao-def)
  (postmodern:!foreign "rssfeed" "feed" "id" :on-delete :cascade :on-update :cascade))


; (postmodern:create-table 'rss-item-store)

(defclass user ()
  ((id)
   (foreign-id)
   (email)
   (first-name)
   (gender)
   (last-name)
   (link)
   (locale)
   
   )
  )
