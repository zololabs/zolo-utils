(ns zolodeck.utils.test
  (:use [clojure.test :only [run-tests deftest is are testing]]))

(defn is-same-sequence? [seq-a seq-b]
  (is (= (sort seq-a) (sort seq-b))))