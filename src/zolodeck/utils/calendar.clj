(ns zolodeck.utils.calendar
  (:use zolodeck.utils.debug
        [clj-time.format :only (parse unparse formatters formatter)]
        [clj-time.core :only (date-time year month day)]
        [clj-time.coerce :only (to-date-time)])
  (:import com.joestelmach.natty.Parser
           java.util.TimeZone
           java.util.Locale
           java.util.Date
           java.text.SimpleDateFormat
           org.joda.time.format.DateTimeFormatterBuilder
           org.joda.time.Weeks)
  (:require [clj-time.core :as time]))

(Locale/setDefault Locale/US)
(TimeZone/setDefault (TimeZone/getTimeZone "GMT"))

(def yyyy-MM-dd-HH-mm "yyyy-MM-dd HH:mm")

(defn date-string->instant [format date-string]
  (when date-string
    (.toDate (parse (or (formatters format) (formatter format)) date-string))))

(defn millis->instant [millis]
  (java.sql.Timestamp. millis))

(defn seconds->instant [seconds]
  (java.sql.Timestamp. (* 1000 seconds)))

(defn seconds->joda-time [seconds]
  (-> seconds seconds->instant to-date-time))

(defn millis-string->instant [millis-string]
  (millis->instant (Long/parseLong millis-string)))

(defn seconds-string->instant [seconds-string]
  (seconds->instant (Long/parseLong seconds-string)))

(def BEGINNING-OF-TIME (date-time 1971 1 1))

(def BEGINNING-OF-TIME-MILLIS (.getMillis BEGINNING-OF-TIME))

(defn now []
  (System/currentTimeMillis))

(defn now-instant []
  (millis->instant (now)))

(defn to-seconds [string-or-millis]
  (condp = (class string-or-millis)
    java.lang.Long (int (/ string-or-millis 1000))
    java.lang.String (/ (.getTime (date-string->instant "yyyy-MM-dd" string-or-millis)) 1000)
    :else (throw (RuntimeException. (str string-or-millis " is not either a String or a Long")))))

(defn fuzzy-parse [date-string]
  (let [groups (.parse (Parser.) date-string)]
    (if-not (empty? groups)
      (first (.getDates (first groups))))))

(defn this-year []
  (-> (now) to-date-time year))

(defn parse-birthday [date-string]
  (if date-string
    (let [d (fuzzy-parse date-string)
          dt (to-date-time d)]
      (if dt
        (if (= (year dt) (this-year))
          (.toDate (date-time 1900 (month dt) (day dt)))
          d)))))

(defn simple-date-format
  ([format-string]
     (simple-date-format format-string "UTC"))
  ([format-string tz-string]
     (doto ^SimpleDateFormat (SimpleDateFormat. format-string)
           (.setTimeZone (TimeZone/getTimeZone tz-string)))))

(def NICE-DATE-FORMATTER 
  (-> (DateTimeFormatterBuilder.)
      (.appendDayOfMonth 2)
      ( .appendLiteral " ")
      .appendMonthOfYearShortText
      ( .appendLiteral ", ")
      (.appendYear 4 4) 
      .toFormatter))

(defn joda-dt-to-nice-string [dt]
  (unparse NICE-DATE-FORMATTER dt))

(defn date-to-nice-string [dt]
  (joda-dt-to-nice-string (to-date-time dt)))

(defn utc-datetime-format
 "Return a 'yyyy-MM-dd HH:mm' date format enforcing UTC semantics. Not thread safe!"
 ^SimpleDateFormat []
 (simple-date-format yyyy-MM-dd-HH-mm))

(defn date-to-string
  "Converts date to yyyy-MM-dd format"
  ([^Date d]
     (date-to-string d (utc-datetime-format)))
  ([^Date d ^SimpleDateFormat formatter]
     (if d (.format formatter d))))

(defn date-to-simple-string [d]
  (if d
    (date-to-string d (simple-date-format "yyyy-MM-dd"))))

(defn year-from-instant [instant]
  (.getYear (to-date-time instant)))

(defn month-from-instant [instant]
  (.getMonthOfYear (to-date-time instant)))

(defn week-from-instant [instant]
  (.getWeekOfWeekyear (to-date-time instant)))

(defn get-year-month-week [instant]
  (let [dt (to-date-time instant)]
    [(.getYear dt) (.getMonthOfYear dt) (.getWeekOfWeekyear dt)]))

(defn weeks-since
  ([]
     (weeks-since (time/now)))
  ([ts]
     (let [ts (to-date-time ts)
           n (time/now)]
       (.getDays (.daysBetween ts n)))))

(defn weeks-between [dt1 dt2]
  (.getWeeks (Weeks/weeksBetween (to-date-time dt1) (to-date-time dt2))))

