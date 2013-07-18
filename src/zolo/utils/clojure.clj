(ns zolo.utils.clojure
  (:use zolo.utils.debug
        clojure.set)
  (:require [zolo.utils.logger :as logger])
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

(defmacro -not-nil-> [first-form & rest-forms]
  `(if-let [ff# ~first-form]
     (-> ff# ~@rest-forms)))

(defmacro -not-nil->> [first-form & rest-forms]
  `(if-let [ff# ~first-form]
     (->> ff# ~@rest-forms)))

(defmacro -not-nil!-> [first-form & rest-forms]
  `(if-let [ff# ~first-form]
     (-> ff# ~@rest-forms)
     (throw (IllegalArgumentException. "Nil is passed"))))

(defmacro -not-nil!->> [first-form & rest-forms]
  `(if-let [ff# ~first-form]
     (->> ff# ~@rest-forms)
     (throw (IllegalArgumentException. "Nil is passed"))))

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

(defn take-randomely [n things]
  (map #(rand-nth %) (repeat (min (count things) n) things)))

(defn take-unique-randomely [n things]
  (let [unique-things (set things)
        c (min n (count unique-things))]
    (reduce (fn [acc things]
              (conj acc (rand-nth (remove acc things))))
            #{}
            (repeat c unique-things))))

(defn random-guid []
  (java.util.UUID/randomUUID))

(defn random-guid-str []
  (str (random-guid)))

(def random-str random-guid-str)

(defn random-integer []
  (rand-int 1e8))

(defn parse-int [s]
  (if (number? s)
    s
    (Integer/parseInt s)))

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

(defn conj-at-end [x coll]
  (concat coll [x]))

(defn blank? [x]
  (cond
   (sequential? x) (empty? x)
   :else (nil? x)))

(defn to-uuid [s]
  (when s
    (if (uuid? s)
      s
      (java.util.UUID/fromString s))))

(defn squeeze [coll]
  (remove blank? coll))

(defn doeach [function coll]
  (doseq [c coll]
    (function c)))

(defn domap [function coll]
  (doall (map function coll)))

(defn distinct-by [func coll]
  (let [f #(= (func %1) (func %2))
        step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (some #(f x %) seen)
                       (recur (rest s) seen)
                       (cons x (step (rest s) (conj seen x))))))
                 xs seen)))]
    (step coll #{})))

(def domapcat (comp doall mapcat))

(defn pmapcat [f batches]
  (->> batches
       (pmap f)
       (apply concat)
       doall))

(defn pdoeach
  ([f n verbose? coll]
     (let [size (count coll)
           indices (range 1 (inc size))
           m (zipmap indices coll)
           batches (partition-all n indices)
           item-processor #(do
                             (if verbose?
                               (print-vals (str "pdoeach: [" (.getName (Thread/currentThread)) "] " % " of " size)))
                             (f (m %)))
           batch-processor #(domap item-processor %)]
       (pmapcat batch-processor batches)))
  ([f n coll]
     (pdoeach f n false coll))
  ([f coll]
     (pdoeach f 1 coll)))

(defn paginate [limit offset data-seq]  
  (->> data-seq
       (drop (or offset 0))
       (take (or limit 50))))

(def apply-pagination paginate)

(defmacro unless-log [binding-vector msg & body]
  `(if-let ~binding-vector
     (do ~@body)
     (do
       (logger/info ~msg)
       nil)))