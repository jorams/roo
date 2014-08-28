(defpackage #:roo.util
  (:use #:cl)
  (:export #:*config*
           #:config
           #:load-configuration))

(in-package #:roo.util)

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
  (getf config name))
