(ns zolodeck.utils.string
  (:import [java.util.regex Pattern]))

(defn split [re s]
  ;; In clj 1.4 this is not working 
  ;; (clojure.string/split #" " auth-token)
  (if s
    (seq  (.split (Pattern/compile re) s))))

(defn underscore->dash  [s]
  (clojure.string/replace s "_" "-"))