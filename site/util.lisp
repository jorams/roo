(in-package #:roo-site)

(defvar *day-names*
  '("Maandag" "Dinsdag" "Woensdag" "Donderdag" "Vrijdag" "Zaterdag" "Zondag"))

(defgeneric name-of-day (date))

(defmethod name-of-day ((date local-time:timestamp))
  (nth (1- (local-time:timestamp-day-of-week date))
       *day-names*))

(defmethod name-of-day (date)
  (name-of-day (roo-parser:datestring->timestamp date)))

(defgeneric pretty-date-string (date))

(defmethod pretty-date-string ((date local-time:timestamp))
  (local-time:format-timestring NIL date
                                :format '(:year "-" :month "-" :day)))

(defmethod pretty-date-string ((date roo-parser:appointment))
  (pretty-date-string (roo-parser:date date)))

(defmethod pretty-date-string (date)
  (pretty-date-string (roo-parser:datestring->timestamp date)))
