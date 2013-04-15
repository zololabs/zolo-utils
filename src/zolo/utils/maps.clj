(ns zolo.utils.maps
  (:use zolo.utils.debug)
  (:require [clojure.walk :as walk]))

(defn transform-vals-with [a-map transform-fn]
  (apply merge (map (fn [[k v]] {k (transform-fn k v)}) a-map)))

(defn transform-keys-with [a-map key-transform]  
  (apply hash-map (mapcat (fn [[k v]] [(key-transform k) v]) a-map)))

(defn transform-key-vals-with [a-map key-transform val-transform]  
  (apply hash-map (mapcat (fn [[k v]] [(key-transform k) (val-transform v)]) a-map)))

(def stringify-keys walk/stringify-keys)

(defn stringify-vals [a-map]
  (transform-vals-with a-map (fn [k s] (if (string? s) s (str s)))))

(defn stringify-map [a-map]
  (-> a-map
      walk/stringify-keys
      stringify-vals))

(defn remove-nil-vals [a-map]
  (apply hash-map (mapcat (fn [[_ v :as kv]] (if v kv)) a-map)))

(defn- kv-updater-for-key [key-tester key-updater [k v]]
  (if-let [new-key (key-updater k)]
    (if (key-tester k)
      {new-key v}
      {k v})))

(defn to-underscore-keys [m]
  (let [f (fn [[k v]] (if (keyword? k) [(clojure.string/replace (name k) "-" "_") v] [k v]))]
    ;; only apply to maps
    (walk/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

;;TODO need to add tests

(defn update-map-keys [m key-tester key-updater]
  (apply merge (map (partial kv-updater-for-key key-tester key-updater) m)))

(defn update-all-map-keys [m key-updater]
  (update-map-keys m (constantly true) key-updater))

(defn keywordize-string [s]
  (-> s
      name      
      (clojure.string/replace "_" "-")
      keyword))

(defn keywordize-map [m]
  (update-all-map-keys m keywordize-string))

(defn select-keys-if [m k-v-pred]
  (->> m
       (filter #(k-v-pred (key %) (val %)))
       (mapcat #(list (key %) (val %)))
       (apply hash-map)))

(defn update-val [m k updater-fn]
  (assoc m k (updater-fn m k (m k))))

(defn group-first-by [attrib objects]
  (-> (group-by attrib objects)
      (transform-vals-with (fn [_ v] (first v)))))

(defn partition-into [n m]
  (->> m
       (partition-all n)
       (map #(apply concat %))
       (map #(apply hash-map %))))

(defn update-in-when [m ks pred f-or-v]
  (let [s (get-in m ks)
        u (map #(if (pred %) (if (fn? f-or-v) (f-or-v %) f-or-v) %) s)]
    (assoc-in m ks u)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
 
  (deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))
