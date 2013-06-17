(ns zolo.utils.string
  (:require [ring.util.codec :as codec]
            [clojure.string :as clj-string]
            [clojure.walk :as walk])
  (:import [java.util.regex Pattern]))

(defn quoted [s]
  (str "'" s "'"))

(def q quoted)

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
  ([^String param-string encoding]
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
  ([^String param-string]
     (parse-query-string param-string "UTF-8")))

(defn make-query-string [m]
  (->> (for [[k v] m]
         (str (name k) "=" (codec/url-encode v)))
       (interpose "&")
       (apply str)))

(defn to-string [e]
  (if (keyword? e)
    (name e)
    (str e)))

(defn up-key-name [kw]
  (.toUpperCase (name kw)))

(defn capitalize [s]
  (apply str (Character/toUpperCase (first (name s))) (rest (name s))))

(defn decode-base64 [string]
  (-> string
      javax.xml.bind.DatatypeConverter/parseBase64Binary
      (String.)))

(defn snippet [text length]
  (if (<= (count text) length)
    text
    (str (subs text 0 length) "...")))
