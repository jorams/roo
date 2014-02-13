(asdf:defsystem #:roo
  :serial t
  :description "Windesheim roosters, but better."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:drakma #:st-json
               #:caramel #:local-time
               #:cl-ppcre
               #:ningle #:clack-errors
               #:spinneret #:split-sequence)
  :components ((:file "packages")
               (:module "parser"
                :components ((:file "parser")))
               (:module "site"
                :components ((:file "util")
                             (:file "views")
                             (:file "site")))))
