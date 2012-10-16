(ns zolodeck.utils.clojure
  (:use clojure.set)
  (:import java.io.File))

(defn create-runonce [function] 
  (let [sentinel (Object.)
        result (atom sentinel)] 
    (fn [& args]
      (locking sentinel 
        (if (= @result sentinel)
          (reset! result (apply function args)) 
          @result)))))

(defmacro defrunonce [fn-name args & body]
  `(def ~fn-name (create-runonce (fn ~args ~@body))))

(defmacro it-> [& [first-expr & rest-expr]]
  (if (empty? rest-expr)
    first-expr
    `(if-let [~'it ~first-expr]
       (it-> ~@rest-expr))))

(defn name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
    [attr macro-args]          (if (map? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [{} macro-args])
    attr                       (if docstring
                                 (assoc attr :doc docstring)
                                 attr)
    attr                       (if (meta name)
                                 (conj (meta name) attr)
                                 attr)]
    [(with-meta name attr) macro-args]))

(defmacro defnk
 "Define a function accepting keyword arguments. Symbols up to the first
 keyword in the parameter list are taken as positional arguments.  Then
 an alternating sequence of keywords and defaults values is expected. The
 values of the keyword arguments are available in the function body by
 virtue of the symbol corresponding to the keyword (cf. :keys destructuring).
 defnk accepts an optional docstring as well as an optional metadata map."
 [fn-name & fn-tail]
 (let [[fn-name [args & body]] (name-with-attributes fn-name fn-tail)
       [pos kw-vals]           (split-with symbol? args)
       syms                    (map #(-> % name symbol) (take-nth 2 kw-vals))
       values                  (take-nth 2 (rest kw-vals))
       sym-vals                (apply hash-map (interleave syms values))
       de-map                  {:keys (vec syms)
                                :or   sym-vals}]
   `(defn ~fn-name
      [~@pos & options#]
      (let [~de-map (apply hash-map options#)]
        ~@body))))

(defn select-randomly [& things]
  (nth things (rand-int (count things))))

(defn random-guid []
  (java.util.UUID/randomUUID))

(defn random-integer []
  (rand-int 1e8))

(defn parse-int [s]
  (Integer/parseInt s))

(defn diff [old-seq new-seq id-fn]
  (let [old-ids (set (map id-fn old-seq))
        new-ids (set (map id-fn new-seq))
        added-ids (difference new-ids old-ids)
        deleted-ids (difference old-ids new-ids)
        remaining-ids (intersection new-ids old-ids)
        old-grouped (group-by id-fn old-seq)
        new-grouped (group-by id-fn new-seq)]
    {:added (mapcat new-grouped added-ids)
     :deleted (mapcat old-grouped deleted-ids)
     :remaining (mapcat new-grouped remaining-ids)}))

(defn clj?
  "Returns true if file is a normal file with a .clj extension."
  [^File file]
  (and (.isFile file)
       (.endsWith (.getName file) ".clj")))

(defn file ^File [path] (if (string? path) (File. ^String path) path))

(defn path->ns [^String path]
  (-> path
    (.replaceAll "/" "\\.")
    (.replaceAll "_" "-")
    (.replaceAll ".clj$" "")))

(defn find-clj-files-in-dir
  [^String dir]
  ;; Use sort by absolute path to get breadth-first search.
  (map #(.getPath ^File %1)
    (sort-by #(.getAbsolutePath ^File %)
      (filter clj? (file-seq (file dir))))))

(defn find-ns-in-dir
  "Searches dir recursively for (ns ...) declarations in Clojure
  source files; returns the symbol names of the declared namespaces."
  [^String dir]
  (let [skip (inc (count dir))]
    (map symbol
      (map path->ns
        (map #(subs %1 skip)
             (find-clj-files-in-dir dir))))))

(defn uuid? [a]
  (instance? java.util.UUID a))

(defn date? [a]
  (instance? java.util.Date a))

(defn collection? [a]
  (instance? java.util.Collection a))

(defn reverse-sort-by [key-fn coll]
  (-> (sort-by key-fn coll)
      reverse))