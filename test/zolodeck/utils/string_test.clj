(ns zolodeck.utils.string-test
  (:use zolodeck.utils.string
        [clojure.test :only [run-tests deftest is are testing]]))

(deftest test-to-string
  (are [expected value] (= expected (to-string value))

       ""    nil
       ""    ""
       "1"   1
       "a"   :a
       "a"   "a"))