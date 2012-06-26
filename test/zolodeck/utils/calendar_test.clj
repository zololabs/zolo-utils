(ns zolodeck.utils.calendar-test
  (:use zolodeck.utils.calendar
        [clj-time.format :only (parse unparse formatters formatter)]
        [clj-time.coerce :only (to-date-time)]
        [clojure.test :only [run-tests deftest is are testing]]))

(defn to-string [judate]
  (unparse (formatter "MM/dd/yyyy") (to-date-time judate)))

(deftest test-parse-birthday
  (is (= "05/22/1900"  (to-string (parse-birthday "5/22"))))
  (is (= "05/22/1980"  (to-string (parse-birthday "5/22/80"))))
  (is (nil? (parse-birthday nil)))
  (is (nil? (parse-birthday "5/22/"))))