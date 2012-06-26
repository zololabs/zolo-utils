(ns zolodeck.utils.test
  (:use [clojure.test :only [run-tests deftest is are testing]]))

(defn is-same-sequence? [seq-a seq-b]
  (is (= (set seq-a) (set seq-b))))

;; A Simple macro that enable to mark your test
;; to pending
(defn repeat-str- [n x]
  (apply str (repeat n x)))

(defn pending-str- [name]
  (apply str
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") 
         "\n========\n" name " is pending !!\n========\n"
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") "\n"
         (repeat-str- 100 "*") "\n"))

(defmacro deftest-pending [name & body]
  (let [message (pending-str- name)]
    `(deftest ~name
       (println ~message))))

(defmacro testing-pending [name & body]
  (let [message (pending-str- name)]
    `(testing ~name
       (println ~message))))