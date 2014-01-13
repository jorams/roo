(in-package #:roo-site)

(defvar *server*)

(defvar *proper-class-name*)
(defvar *datestring*)

(defun index-route ()
  (index))

(defun redir-route ()
  (let ((class (hunchentoot:post-parameter "class")))
         (if class
           (multiple-value-bind (id class) (roo-parser:class-id class)
             (if class
               (hunchentoot:redirect (format NIL "/~A" class) :code hunchentoot:+http-see-other+)
               (class-not-found)))
           (class-not-found))))

(defun schedule-route ()
  (multiple-value-bind (class proper-name)
    (roo-parser:class-id (getf *route-params* :class))
    (if class
      (let* ((date (or (getf *route-params* :date)
                       (roo-parser:datestring)))
             (appointments (roo-parser:get-appointments class date))
             (*proper-class-name* proper-name)
             (*datestring* date))
        (render-schedule appointments proper-name (roo-parser:raw-url class date)))
      (class-not-found))))

(map-routes 
  ("/" :get index-route :post redir-route)
  ("/:class/:date" :get schedule-route :date "[0-9]{8}")
  ("/:class" :get schedule-route))

(defclass acceptor (hunchentoot:acceptor) ())

(defmethod hunchentoot:acceptor-status-message
  ((acceptor acceptor) http-return-code &key &allow-other-keys)
  (error-status http-return-code))

(defun start ()
  (unless (boundp 'roo-parser:*classes*)
    (roo-parser:refresh-classes))
  (unless (boundp '*server*) 
    (setf *server* (make-instance 'acceptor :port 8000))
    (hunchentoot:start *server*)))
