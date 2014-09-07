;;;; Roo, better schedules for Windesheim.
;;;; Copyright (C) 2014  Joram Schrijver
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
               #:clack #:ningle)
  :components ((:file "util")
               (:module "parser"
                :components ((:file "package")
                             (:file "parser")))
               (:module "site"
                :components ((:file "package")))))
