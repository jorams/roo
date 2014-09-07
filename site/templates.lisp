(in-package #:roo.site)

(djula:add-template-directory
 (asdf:system-relative-pathname :roo "site/templates/"))

(defmacro deftemplate (name file)
  `(defparameter ,name
     (djula:compile-template* ,file)))

(defmacro render-template (template &rest template-arguments)
  (with-gensyms (stream)
   `(let ((,stream (make-string-output-stream)))
      (djula:render-template* ,template ,stream ,@template-arguments)
      (get-output-stream-string ,stream))))

(deftemplate +index+ "index.html")
