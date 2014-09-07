(defpackage #:roo.parser
  (:use #:cl #:roo.util)
  (:import-from :alexandria
                #:lastcar)
  (:export #:*groups*
           #:*teachers*
           #:timetable
           #:update!
           #:id
           #:name
           #:long-name
           #:display-name
           #:departments
           #:id
           #:lesson-number
           #:lesson-id
           #:lesson-type
           #:lesson-text
           #:date
           #:start-time
           #:end-time
           #:elements))
