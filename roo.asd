(asdf:defsystem #:roo
  :serial t
  :description "Better schedules for Windesheim"
  :author "Joram Schrijver <i@joram.io>"
  :license "GNU AGPL 3"
  :depends-on (;; parser and site
               #:local-time #:cl-ppcre #:yason #:alexandria
               ;; parser
               #:drakma
               ;; site
               #:restas #:hunchentoot)
  :components ((:file "util")
               (:module "parser"
                :components ((:file "package")
                             (:file "parser")))
               (:module "site"
                :components ((:file "package")))))
