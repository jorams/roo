(in-package #:roo-parser)

(defparameter *base-url*
  "https://roosters.windesheim.nl/WebUntis/Timetable.do?type=1")

;;; This is to make Drakma recognize the json and html as text
(pushnew '("application" . "json") drakma:*text-content-types*)
(pushnew '("text" . "html") drakma:*text-content-types*)

(defvar *classes*)
(defparameter *cookie-jar*
  (make-instance 'drakma:cookie-jar
                 :cookies (list
                            (make-instance 'drakma:cookie
                                           :name "schoolname"
                                           :value "\"_V2luZGVzaGVpbQ==\""
                                           :domain "roosters.windesheim.nl"))))

(local-time:define-timezone 
  amsterdam 
  (merge-pathnames #p"Europe/Amsterdam"
                   local-time::*default-timezone-repository-path*))

(defclass appointment ()
  ((date :initarg :date
        :accessor date)
   (start-time :initarg :start
               :accessor start-time)
   (end-time :initarg :end
             :accessor end-time)
   (lessons :initarg :lessons
                 :initform '()
                 :accessor lessons))
  (:documentation "Represents a lesson. The actual"))

(defclass lesson ()
  ((class :initarg :class)
   (teacher :initarg :teacher
            :accessor teacher)
   (course :initarg :course
         :accessor course)
   (location :initarg :location
             :accessor location)
   (subject :initarg :subject
           :accessor subject))
  (:documentation "Represents an instance of a lesson, mostly for when a
                   lesson is given to parts of a class separately"))

(defmacro request-url (url &rest params)
  "Requests the URL using drakma, automatically supplying the cookie jar"
  `(drakma:http-request ,url
                        ,@params
                        :cookie-jar *cookie-jar*))

(defun datestring (&optional datestring)
  "If datestring is supplied and looks like a valid datestring (8 numbers),
   returns datestring. Otherwise returns a datestring of the current date"
  (if (ppcre:scan "[0-9]{8}" datestring)
    datestring
    (let ((today (local-time:today)))
      (local-time:format-timestring NIL today :format '((:year 4) (:month 2) (:day 2))))))

(defun datestring->timestamp (datestring)
  "Converts a `datestring` to a local-time timestamp"
  (multiple-value-bind (match regs) (ppcre:scan-to-strings
                                      "([0-9]{4})([0-9]{2})([0-9]{2})"
                                      datestring) 
    (let ((d (parse-integer (aref regs 2)))
          (m (parse-integer (aref regs 1)))
          (y (parse-integer (aref regs 0))))
   (local-time:encode-timestamp 0 0 0 0 d m y :timezone amsterdam))))

(defun refresh-classes ()
  "sets *classes* to an alist of (classname . classid) pairs"
  (let ((json (st-json:read-json 
                (request-url *base-url*))))
     (setf *classes*
           (mapcar #'(lambda (item)
                       (cons (st-json:getjso "name" item)
                             (st-json:getjso "id" item)))
                   (st-json:getjso "data.items" 
                                   (st-json:getjso 
                                     "cargs"
                                     (nth 7
                                          (st-json:getjso* 
                                            "viewModel.dojoObjects" 
                                            json))))))))

(defun raw-url (class date)
  (concatenate 'string *base-url* "&id=" (write-to-string class)
                            "&ajaxCommand=renderTimetable&date=" date))

(defun rooster-dom (id &optional date)
  "Returns the schedule of the class with the supplied id at the specified
   date. If no date is specified the server will use the current date"
  (caramel:html-resource
   (request-url (raw-url id date))))

(defun class-id (name)
  "Returns the id associated with the class name. Also returns as a second
   value a string containing the class name in the proper case"
  (unless (boundp '*classes*) (refresh-classes))
  (let ((class-id (assoc name *classes* :test #'string-equal)))
    (values (cdr class-id) (car class-id))))

(defun appointments-dom (rooster-dom)
  "Returns all DOM-nodes (of type td) representing lessons."
  ; The two classes I select here seem to be enough to get all <td>s
  ; representing lessons.
  (caramel:select "td.A_0_1,td.A_0_6" rooster-dom))

(defun text (domelem)
  "Returns the text contained within a node, roughly."
  (caramel:get-content (car (caramel:get-content domelem))))

(defun create-appointment (ldom)
  "Turns an appointment DOM element into an instance of APPOINTMENT"
  (let* ((start-end-time (caramel:select ".ti" ldom))
         (start-time (car start-end-time))
         (end-time (cadr start-end-time))
         (date (parse-integer (ppcre:scan-to-strings "[0-9]{8}" (caramel:get-attr ldom "onclick")))))
    (make-instance 'appointment
                   :start (text start-time)
                   :end (text end-time)
                   :date date
                   :lessons
                   (loop for class in (caramel:select ".Z_0_0" ldom)
                         for teacher in (caramel:select ".Z_1_0" ldom)
                         for course in (caramel:select ".Z_2_0" ldom)
                         for location in (caramel:select ".Z_2_1" ldom)
                         for subject in (caramel:select ".Z_3_0" ldom)
                         collect (make-instance 'lesson
                                                :class (text class)
                                                :teacher (text teacher)
                                                :course (text course)
                                                :location (text location)
                                                :subject (text subject))))))

(defun get-appointments (id &optional date)
  "Returns a list of lessons for the class with the specified id. If the
   specified ID is a string it is automatically passed through CLASS-ID to get
   the id"
  (stable-sort
   (mapcar #'create-appointment (appointments-dom (rooster-dom (if (stringp id)
                                                      (class-id id)
                                                      id) 
                                                    date)))
   #'< :key #'date))
