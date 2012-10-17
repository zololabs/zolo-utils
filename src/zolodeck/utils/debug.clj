(ns zolodeck.utils.debug
  (:use clojure.pprint))

(defn snip-string [s]
  (-> s
      (.substring 1 (dec (dec (.length s))))
      (str "\n")))

(defn snipped-pretty-string [& things]
  (let [s (with-out-str (pprint things))]
    (snip-string s)))

(defn print-vals [& args]
  (apply println (cons "*** " (map #(if (string? %) % (with-out-str (pprint %)))  args)))
  (last args))

(defn print-vals-> [obj msg]
  (print-vals msg obj)
  obj)

(defmacro try-catch [& body]
  `(try
     (do ~@body)
     (catch Exception ~'e
       (print-vals "EXCEPTION:" ~'e))))

(defn scaffold
  "Print the ancestor method signatures of a given interface."
  [iface]
  (doseq [[iface methods] (->> iface
                               .getMethods
                               (map #(vector (.getName (.getDeclaringClass %))
                                             (symbol (.getName %))
                                             (count (.getParameterTypes %))))
                               (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
       (str "    "
            (list name (into ['this] (take argcount (repeatedly gensym)))))))))