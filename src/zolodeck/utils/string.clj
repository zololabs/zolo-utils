(ns zolodeck.utils.string
  (:require [ring.util.codec :as codec]
            [clojure.string :as clj-string]
            [clojure.walk :as walk])
  (:import [java.util.regex Pattern]))

(defn split [re s]
  ;; In clj 1.4 this is not working 
  ;; (clojure.string/split #" " auth-token)
  (if s
    (seq  (.split (Pattern/compile re) s))))

(defn underscore->dash  [s]
  (clojure.string/replace s "_" "-"))

(defn- assoc-param
  "Associate a key with a value. If the key already exists in the map,
  create a vector of values."
  [map key val]
  (assoc map key
    (if-let [cur (map key)]
      (if (vector? cur)
        (conj cur val)
        [cur val])
      val)))

(defn parse-query-string
  "Parse parameters from a string into a map."
  [^String param-string encoding]
  (let [stringified (reduce
                     (fn [param-map encoded-param]
                       (if-let [[_ key val] (re-matches #"([^=]+)=(.*)" encoded-param)]
                         (assoc-param param-map
                                      (codec/url-decode key encoding)
                                      (codec/url-decode (or val "") encoding))
                         param-map))
                     {}
                     (clj-string/split param-string #"&"))]
    (walk/keywordize-keys stringified)))