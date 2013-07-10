(ns zolo.utils.logger
  (:require [clojure.tools.logging :as logger]
            [zolo.utils.string :as string-utils])
  (:use zolo.utils.debug
        clojure.pprint)
  (:import [org.slf4j MDC]))

(defmacro trace [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/trace v#)
     (last l#)))

(defmacro trace-> [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/trace v#)
     (first l#)))

(defmacro debug [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/debug v#)
     (last l#)))

(defmacro debug-> [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/debug v#)
     (first l#)))

(defmacro info [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/info v#)
     (last l#)))

(defmacro info-> [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/info v#)
     (first l#)))

(defmacro warn [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/warn v#)
     (last l#)))

(defmacro warn-> [& args]
  `(let [l# (list ~@args)
         v# (apply print-str l#)]
     (logger/warn v#)
     (first l#)))

(defmacro error
  "Error level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logger/error ~@args))

(defmacro fatal
  "Fatal level logging using print-style args."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logger/fatal ~@args))

(defmacro with-logging-context [x & body]
  `(let [x# ~x
         ctx# (into {} (. ~MDC getCopyOfContextMap))]
     (try
       (if (map? x#)
         (doall (map (fn [[k# v#]] (. ~MDC put (name k#) (string-utils/to-string v#))) x#)))
       ~@body
       (finally
        (if (map? x#)
          (doall (map (fn [[k# v#]]
                        (. ~MDC remove (name k#))
                        (when-let [old# (get ctx# (name k#))]
                          (. ~MDC put (name k#) old#))) x#)))))))


