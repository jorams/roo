(in-package #:roo-site)

(defvar *day-names*
  '("Maandag" "Dinsdag" "Woensdag" "Donderdag" "Vrijdag" "Zaterdag" "Zondag"))

(defun name-of-day (date)
  (nth (local-time:timestamp-day-of-week
             (roo-parser:datestring->timestamp (format NIL "~A" date)))
       *day-names*))
