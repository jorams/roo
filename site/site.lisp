(in-package #:roo.site)

(defvar *group-names* ()
  "A list of all the names of groups")
(defvar *teacher-names* ()
  "A list of all the names of teachers")

(defvar *group-ids* (make-hash-table :test #'equalp)
  "An EQUALP hash table mapping group names to their id.")
(defvar *teacher-ids* (make-hash-table :test #'equalp)
  "An EQUALP hash table mapping teacher names to their id.")

(defun update-mappings! ()
  (macrolet ((update-from (var ids names)
               `(maphash (lambda (id val)
                           (setf (gethash (display-name val) ,ids)
                                 id)
                           (push (display-name val) ,names))
                         ,var)))
    (update!)
    (clrhash *group-ids*)
    (clrhash *teacher-ids*)
    (update-from *groups* *group-ids* *group-names*)
    (update-from *teachers* *teacher-ids* *teacher-names*)))

(mount-module -static- (#:restas.directory-publisher)
  (:url "static")
  (restas.directory-publisher:*directory*
   (asdf:system-relative-pathname :roo "site/static/")))

(define-route index ("")
  (render-template +index+ :groups *group-names*
                           :teachers *teacher-names*))
