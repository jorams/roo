(in-package #:roo-site)

(defvar *handler*)
(defvar *app* (make-instance '<app>))

(defvar *proper-class-name*)
(defvar *date*)

(setf (route *app* "/") #'index)

(setf (route *app* "/" :method :POST)
      (lambda (params)
        (let ((class (getf params :|class|)))
          (if class
	      (multiple-value-bind (id class) (roo-parser:class-id class)
		(if class
		    (clack.response:redirect *response*
					     (format NIL "/~A" class)
					     303)
		    (class-not-found)))
	      (class-not-found)))))

(defun schedule-route (params)
  (multiple-value-bind (class proper-name)
      (roo-parser:class-id (getf params :class))
    (if class
	(let* ((date (or (getf params :date)
			 (roo-parser:datestring)))
	       (appointments (roo-parser:get-appointments class date))
	       (*proper-class-name* proper-name)
	       (*date* (roo-parser:datestring->timestamp date)))
	  (if (not *date*) (return-from schedule-route))
	  (render-schedule appointments proper-name (roo-parser:raw-url class date)))
	(class-not-found))))

(setf (route *app* "/:class/:date") #'schedule-route)
(setf (route *app* "/:class") #'schedule-route)

(defmethod not-found ((this (eql *app*)))
  (declare (ignore this))
  (setf (clack.response:status *response*) 404)
  (error-status 404))

(defun start (&optional dev?)
  (unless (boundp 'roo-parser:*classes*)
    (roo-parser:refresh-classes))
  (if (not (boundp '*handler*))
      (setf *handler*
	    (clack:clackup (clack.builder:builder
			    (clack-errors:<clack-error-middleware>
                             :debug dev?
                             :prod-renderer (lambda (condition env)
                                              (declare (ignore condition env))
                                              (error-status 500)))
			    (clack.middleware.static:<clack-middleware-static>
                             :path "/static/"
                             :root #p"site/static/")
			    *app*)
			   :port 5000))
      (princ "Already started.")))

(defun stop ()
  (clack.handler:stop *handler*)
  (makunbound '*handler*))
