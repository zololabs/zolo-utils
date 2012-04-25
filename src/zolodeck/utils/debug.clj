(ns zolodeck.utils.debug
  (:use clojure.pprint))

(defn print-vals [& args]
  (apply println (cons "*** " (map #(if (string? %) % (with-out-str (pprint %)))  args)))
  (last args))

