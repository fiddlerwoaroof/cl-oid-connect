(in-package :whitespace)
(defun base-template-f (&optional demo)
  (cl-markup:xhtml5
    (:head
      (:title "Whitespace")
      (:script :src "https://code.jquery.com/jquery-2.1.4.min.js" :type "text/javascript" "")
      (:script :src "https://cdnjs.cloudflare.com/ajax/libs/angular.js/1.4.5/angular.js" :type "text/javascript" "")
      (:script :src "https://cdnjs.cloudflare.com/ajax/libs/angular.js/1.4.5/angular-resource.js" :type "text/javascript" "")
      (:script :src "https://cdnjs.cloudflare.com/ajax/libs/angular.js/1.4.5/angular-sanitize.js" :type "text/javascript" "")
      (:script :src "/static/js/fold.js" :type "text/javascript" "")
      (:script :src "/static/js/whitespace-angular.js" :type "text/javascript" "")
      (:link :rel "stylesheet" :href "/static/css/reset.css")
      (:link :rel "stylesheet" :href "/static/css/baseline_post.css")
      (:link :rel "stylesheet" :href "/static/css/formalize.css")
      (:link :rel "stylesheet" :href "/static/css/main.css")
      (:link :rel "stylesheet" :href "/static/css/content.css")
      (:link :rel "stylesheet" :href "/theme/light.css")
      (:link :rel "icon" :href "/static/images/Whitespace_favicon.png" :type "image/x-icon")
      (:link :rel "shortcut icon" :href "/static/images/Whitespace_favicon.png" :type "image/x-icon"))

    (:body :ng-app "whitespace" :ng-controller "MainCtrl"
     (:header
       (:button :class "flip-button" "…")
       (:h1 "Whitespace")
       )
     (:section :id "content"
      (:section :id "sidebar"
       (:ul :class "menu" 
        (:li :ng-repeat "feed in feeds.result"
         (:a :ng-click "toggleClosed(feed)" "{{ feed.title }}"))))
      (:main
        (cl-markup:raw
          (unless demo
            (cl-markup:markup
              (:form :name "add-form" :id "add-form" :ng-submit "addFeed()"
               (:input :type "text" :name "url" :class "urltext" :ng-model "addForm.url" :placeholder "http://example.com/feed.rss . . ." "")
               (:input :type "hidden" :name "api" :value "yes" "")
               (:button :type "submit" :class "fsub" "+")))))
        (:img :ng-class "{spinner: true, hide: feeds.result !== undefined}" :src "/static/images/spinner.gif" "")
        (:div :class "hide" :ng-class "{hide: feeds.result === undefined}"
         (:section :ng-class "{feed: true, closed: !feed.closed}" :ng-repeat "feed in feeds.result"
          (:section :class "feed-header" :ng-click "toggleClosed(feed)"
           (:h2 "{{ feed.title }}")
           (:h3 "{{ feed.description }}"))
          (:ul :class "post-list"
           (:li :ng-class "{link: true, closed: !item.closed}" :ng-repeat "item in feed.items"
            (:section :class "link-header" :ng-click "toggleClosed(item)"
             (:h4 "{{item.title}}")
             (:p :class "link-info"
              (:a :target "_blank" :ng-href "{{item.link}}" :class "link-url" "{{item.link}}")
              (:span :class "link-date" "{{item.date}}")))
            (:section :class "link-content"
             (:div :ng-bind-html "renderHtml(item.description)" ""))))))))
     (:footer))))
