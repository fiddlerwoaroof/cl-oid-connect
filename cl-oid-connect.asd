;;;; cl-oid-connect.asd

(asdf:defsystem :cl-oid-connect
  :description "A Common Lisp Implementation of Various OAuth2 Authentication Protocols"
  :author "Ed L <(format nil \"~a@~a\" \"el-projects\" \"howit.is\")>"
  :license "2=Clause BSD"
  :depends-on (:drakma
                :ningle
                :clack
                :cljwt
                :cl-json
                :anaphora
                :alexandria
                :lack-middleware-session
                :sheeple
                :lass
                :lquery
                :plump
                :cl-who
                :postmodern
                :iterate
                :fwoar.lisputils)
  :serial t
  :components ((:file "package")
               (:file "utils")  
               (:file "objects")  
               (:file "cl-oid-connect")))


