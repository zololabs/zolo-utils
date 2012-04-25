(ns zolodeck.utils.calendar
  (:use [clj-time.format :only (parse formatters)]
        [clj-time.core :only (date-time)]))

(defn java-date-from-string [yyyy-MM-dd-string]
  (.toDate (parse (formatters :date) "2012-03-20")))

(def BEGINNING-OF-TIME (date-time 1971 1 1))

(def BEGINNING-OF-TIME-MILLIS (.getMillis BEGINNING-OF-TIME))