(defpackage #:roo-parser
  (:use #:cl)
  (:export :lesson
           :appointment
           :get-appointments
           :class-id
           :refresh-classes
           :*classes*
           :amsterdam
           :datestring
           :datestring->timestamp
           :timestamp->datestring
           :raw-url

           :subject
           :location
           :course
           :teacher
           :start-time
           :end-time
           :date
           :lessons))

(defpackage #:roo-site
  (:use #:cl
        #:ht-routes
        #:spinneret
        #:split-sequence)
  (:export :start))

