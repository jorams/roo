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

(in-package #:roo.parser)

(defparameter +schedule-element-classes+
  '(1 group
    2 teacher
    3 course
    4 classroom)
  "A mapping of WebUntis type numbers to their respective class names.")

(defparameter +schedule-element-ids+
  '(group 1
    teacher 2
    course 3
    classroom 4)
  "A mapping of class names to their WebUntis type numbers")

(defparameter +lesson-types+
  '(1 :free
    2 :lesson
    3 :reservation
    4 :book
    5 :storno ; ?
    6 :lock
    7 :holiday
    8 :holiday-lock
    9 :conflict
    10 :subst
    11 :cancelled
    12 :without-elem
    13 :elem-changed
    14 :shift
    15 :special-duty
    16 :exam
    17 :break-supervision
    18 :stand-by
    19 :office-hour))

(defvar *departments* (make-hash-table))
(defvar *groups* (make-hash-table))
(defvar *teachers* (make-hash-table))
(defvar *courses* (make-hash-table))
(defvar *classrooms* (make-hash-table))

(deftype lesson-type ()
  '(member :free :lesson :reservation :book :storno :lock :holiday
  :holiday-lock :conflict :subst :cancelled :without-elem :elem-changed :shift
  :special-duty :exam :break-supervision :stand-by :office-hour))

(defclass department ()
  ((id :type integer
       :initarg :id
       :reader id)
   (name :type string
         :initarg :name
         :reader name)
   (label :type string
          :initarg :label
          :reader label)))

(defun make-department (json)
  (make-instance 'department
                 :id (gethash "id" json)
                 :name (gethash "name" json)
                 :label (gethash "label" json)))

(defun make-departments (json-objects &optional (departments *departments*))
  (mapc (lambda (json)
          (let ((department (make-department json)))
            (setf (gethash (id department) departments)
                  department)))
        json-objects)
  departments)

(defclass schedule-element ()
  ((id :type integer
       :initarg :id
       :reader id)
   (name :type string
         :initarg :name
         :reader name)
   (long-name :type string
              :initarg :long-name
              :reader long-name)
   (display-name :type string
                 :initarg :display-name
                 :reader display-name)
   (departments :type list
                :initarg :departments
                :reader departments)))

(defclass group (schedule-element) ())
(defclass teacher (schedule-element) ())
(defclass course (schedule-element) ())
(defclass classroom (schedule-element) ())

(deftype schedule-element-name ()
  '(member group teacher course classroom))

(defun make-schedule-element (type json)
  (make-instance type
                 :id (gethash "id" json)
                 :name (gethash "name" json)
                 :long-name (gethash "longName" json)
                 :display-name (gethash "displayName" json)
                 :departments (mapcar (lambda (did) (gethash did *departments*))
                                      (gethash "dids" json))))

(defclass lesson ()
  ((id :type integer
       :initarg :id
       :reader id)
   (lesson-number :type integer
                  :initarg :lesson-number
                  :reader lesson-number)
   (lesson-id :type integer
              :initarg :lesson-id
              :reader lesson-id)
   (lesson-type :type lesson-type
                :initarg :lesson-type
                :reader lesson-type)
   (lesson-text :type string
                :initarg :lesson-text
                :reader lesson-text)
   (date :type integer
         :initarg :date
         :reader date)
   (start-time :type integer
               :initarg :start-time
               :reader start-time)
   (end-time :type integer
             :initarg :end-time
             :reader end-time)
   (elements :initarg :elements
             :reader elements)))

(defun make-lesson (json)
  (flet ((find-lesson-type (is)
           (loop for type in '("standard" "breakSupervision" "officeHour"
                               "standBy" "substitution" "roomSubstitution"
                               "additional" "shift" "cancelled" "withoutElement"
                               "withoutElementAndCanceled" "exam" "free"
                               "confirmed" "unconfirmed" "roomLock" "holiday")
                 for x from 1
                 if (gethash type is)
                   return x))
         (find-element (elem)
           (gethash (gethash "id" elem)
                    (ecase (gethash "type" elem)
                      (1 *groups*)
                      (2 *teachers*)
                      (3 *courses*)
                      (4 *classrooms*)))))
    (make-instance 'lesson
                   :id (gethash "id" json)
                   :lesson-number (gethash "lessonNumber" json)
                   :lesson-id (gethash "lessonId" json)
                   :lesson-text (gethash "lessonText" json)
                   :date (gethash "date" json)
                   :start-time (gethash "startTime" json)
                   :end-time (gethash "endTime" json)
                   :lesson-type (find-lesson-type (gethash "is" json))
                   :elements (mapcar #'find-element (gethash "elements" json)))))

(defun fetch-json (uri &rest post-parameters)
  (let* ((parameters (alexandria:plist-alist post-parameters))
         (stream (drakma:http-request uri
                                      :method :post
                                      :parameters parameters
                                      :want-stream t)))
    (yason:parse stream)))

(defun fetch-elements (uri type)
  (check-type type schedule-element-name)
  (let* ((json (fetch-json uri
                           "ajaxCommand" "getPageConfig"
                           "type" (princ-to-string
                                   (getf +schedule-element-ids+ type type))))
         (*departments* (make-departments (gethash "departments" json))))
    (values (mapcar #'(lambda (json)
                        (make-schedule-element type json))
                    (gethash "elements" json))
            *departments*)))

(defun fetch-timetable (uri element date &key (type (type-of element)))
  (check-type type schedule-element-name)
  (let* ((json (fetch-json
                uri
                "ajaxCommand" "getWeeklyTimetable"
                "elementType" (princ-to-string
                               (getf +schedule-element-ids+ type type))
                "elementId" (princ-to-string
                             (id element))
                "date" (princ-to-string date)
                ;; If we don't supply this the elements list of a lesson won't
                ;; contain the group
                "formatId" "2")))
    (mapcar #'make-lesson
            (gethash (princ-to-string (id element))
                     (gethash "elementPeriods"
                              (gethash "data"
                                       (gethash "result" json)))))))

(defun update-groups (uri)
  (mapcar (lambda (g) (setf (gethash (id g) *groups*) g))
          (fetch-elements uri 'group)))

(defun update-teachers (uri)
  (mapcar (lambda (g) (setf (gethash (id g) *teachers*) g))
          (fetch-elements uri 'teacher)))

(defun update-courses (uri)
  (mapcar (lambda (g) (setf (gethash (id g) *courses*) g))
          (fetch-elements uri 'course)))

(defun update (uri)
  (update-groups uri)
  (update-teachers uri)
  (update-courses uri))
