(ns zolo.utils.maps-test
  (:use [clojure.test :only [run-tests deftest is are testing]])
  (:require [zolo.utils.maps :as maps]))

(deftest update-a-single-value-inside-a-sequence-value
  (are [ m ks p f expected] (= expected (maps/update-in-when m ks p f))
       
       {:a 1 :b {:c 3} :d [{:e 5} {:f 6}]}
       [:d]
       #(= 6 (:f %))
       (constantly {:g 8})
       {:a 1 :b {:c 3} :d [{:e 5} {:g 8}]}

       {:a 1 :b {:c 3} :d [{:e 5} {:f 6}]}
       [:d]
       #(= 6 (:f %))
       {:g 8}
       {:a 1 :b {:c 3} :d [{:e 5} {:g 8}]}


       {:a {:d [{:e 5} {:f 6}]}}
       [:a :d]
       #(= 6 (:f %))
       {:g 10}
       {:a {:d [{:e 5} {:g 10}]}}

       ))