(in-package #:roo-site)

(defmacro render (&body body)
  `(with-output-to-string (*html*)
     ,@body))

(defmacro main-view (content &key (title "Roo") head-tags footer)
  `(with-html 
     (:doctype)
     (:html
     (:head
       (:title ,title)
       (:link :rel "stylesheet" :type "text/css" :href "/static/style.css")
       (:link :rel "shortcut icon" :type "image/x-icon" :href "/favicon.ico")
       (:meta :name "viewport" :content "initial-scale=1")
       ,@head-tags)
     (:body 
       (:header
         (:a :href "/" (:img.logo :src "/static/logo.svg")))
       ,@content
       (:footer 
         (if ,footer (:span.left ,footer))
         (:span.right ("Roo is [open source.](https://github.com/jorams/roo)")))))))

(defun render-lesson (lesson)
  (with-accessors ((teacher roo-parser:teacher)
                   (location roo-parser:location)
                   (subject roo-parser:subject)) lesson
   (with-html (:li.lesson
           (:span.location location " ")
           (:span.subject subject " - ")
           (:span.teacher teacher " ")))))

(defun render-appointment (appointment)
  (with-accessors ((start-time roo-parser:start-time)
                   (end-time roo-parser:end-time)
                   (lessons roo-parser:lessons)) appointment
    (with-html (:div.item
              (:div.time start-time " ⇾ " end-time)
              (:ul.item (loop for l in lessons do (render-lesson l)))))))

(defun render-day (date appointments)
  (with-html (:section.day
           (:h2.item.header (name-of-day date)
            (local-time:format-timestring NIL (roo-parser:datestring->timestamp date)
                                          :format '(:day " " :short-month)))
           (loop for l in appointments do (render-appointment l)))))

(defun render-days (appointments)
  (loop for day in (remove-duplicates appointments
                                      :test #'equal
                                      :key #'roo-parser:date)
        do (let* ((date (roo-parser:date day))
                  (day-appointments (remove-if-not #'(lambda (d) (equal d date))
                                                   appointments
                                                   :key #'roo-parser:date)))
             (render-day date day-appointments))))

(defun render-schedule (appointments class &optional raw-url)
  (let* ((timestamp (roo-parser:datestring->timestamp *datestring*))
         (prev-week-datestring (roo-parser:timestamp->datestring
                                 (local-time:timestamp-
                                   timestamp
                                   7 :day)))
         (next-week-datestring (roo-parser:timestamp->datestring
                                 (local-time:timestamp+
                                   timestamp
                                   7 :day))))
    (render (main-view
              ((:div.prev-next
                 (:a :href (format NIL "/~A/~A" *proper-class-name*
                                   prev-week-datestring) "<<<")
                 (local-time:format-timestring NIL timestamp :format '("Week " :iso-week-number))
                 (:a :href (format NIL "/~A/~A" *proper-class-name*
                                   next-week-datestring) ">>>"))
                (if appointments
                  (render-days appointments)
                  (:p.center "You are looking at the inside of a rooster. An empty one.")))
              :title (format NIL "Rooster ~A - Roo" class)
              :footer (with-html (:a :href raw-url "Raw"))))))

(defun class-input ()
  (with-html 
    (:div.class-input
      (:form :method "POST" :action "/"
       (:input.classes :type "text" :name "class" :placeholder "Class name...")
       (:input.submit :type "submit" :value "Cock-a-doodle-doo!")))

    (:script :type "text/javascript"
     :src "//cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js")
    (:script :type "text/javascript" 
     :src "//cdnjs.cloudflare.com/ajax/libs/typeahead.js/0.9.3/typeahead.min.js")
    (:script :type "text/javascript" 
     (format NIL 
             "$('.classes').typeahead({
              name: 'classes',
              limit: 10,
              local: [~{'~A',~%~}]});"
             (loop for c in roo-parser:*classes* collect (car c))))))

(defun index ()
  (render (main-view
            ((:h1 "Roo")
             (:p ("Roo is an alternative schedule viewer for [Windesheim.](http://www.windesheiminternational.nl/)
                  \"Rooster\" is a Dutch word for \"schedule\"."))
             (class-input)))))

(defun class-not-found ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (render (main-view
            ((:h1 "404 Missing roosters!")
             (:p "Rumors have started spreading of a mysterious \"Rooster 
                  Abductor\". Eye witnesses report the missing roosters never
                  existed in the first place, but we don't believe them. Who is
                  this mysterious individual, and how did they manage to get so
                  many eye-witness accomplices? We don't know, but we'll keep
                  looking for answers. In the meantime you can use the form
                  below to find a rooster that hasn't gone missing yet.")
             (class-input)))))

(defun error-status (status-code)
  (render (main-view
            ((:h1 (format NIL "Error ~A. It's your fault." status-code))
             (:p "(That doesn't necessarily have to be true, but for now I'm blaming you.)"))
            :title (format NIL "~A - Roo" status-code))))
