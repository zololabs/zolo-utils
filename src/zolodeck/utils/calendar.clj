(ns zolodeck.utils.calendar
  (:use [clj-time.format :only (parse formatters formatter)]
        [clj-time.core :only (date-time)]))

(defn date-string->instant [format date-string]
  (.toDate (parse (or (formatters format) (formatter format)) date-string)))

(defn millis->instant [millis]
  (java.sql.Timestamp. millis))

(def BEGINNING-OF-TIME (date-time 1971 1 1))

(def BEGINNING-OF-TIME-MILLIS (.getMillis BEGINNING-OF-TIME))

