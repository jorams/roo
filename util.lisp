(defpackage #:roo.util
  (:use #:cl)
  (:export #:*config*
           #:config
           #:load-configuration
           #:group-by))

(in-package #:roo.util)

;;; Configuration

(defvar *config* nil
  "Global configuration store for Roo. Currently a plist.")

(defun load-configuration (&key
                             (file (asdf:system-relative-pathname
                                    :roo "config.lisp"))
                             (globalp t))
  "Load (or reload) a configuration file.

FILE is the file to read.
GLOBALP defines whether or not *CONFIG* should be set to this newly loaded
configuration.

Returns the loaded plist.
"
  (when (probe-file file)
    (with-open-file (stream file)
      (let ((config-plist (read stream)))
        (when globalp
          (setf *config* config-plist))
        config-plist))))

(defun config (name &optional (config *config*))
  "Get a configuration value.

NAME is the name of the configuration value to fetch.
CONFIG is the configuration variable it should be read from.

Returns the configuration variable, NIL if it doesn't exist."
  (unless *config* (error "Configuration not loaded!"))
  (getf config name))


;;; Miscellaneous

(defun group-by (list key &optional (hash-test #'eql))
  ;; adjust the table test and my-key functions as necessary.
  (loop with table = (make-hash-table :test hash-test)
        for object in list
        do (push object (gethash (funcall key object) table))
        finally (return (alexandria:hash-table-values table))))
