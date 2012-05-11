(ns zolodeck.utils.clojure)

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
