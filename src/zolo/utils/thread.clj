(ns zolo.utils.thread
  (:use zolo.utils.clojure)
  (:require [zolo.utils.logger :as logger])
  (:import [java.util.concurrent ThreadFactory ScheduledThreadPoolExecutor TimeUnit]))

(defn- random-thread-name [prefix]
  (str prefix "-" (random-guid-str)))

(defn thread-factory [thread-name-prefix]
  (proxy [ThreadFactory] []
    (newThread [thunk]
      (Thread. thunk (random-thread-name thread-name-prefix)))))

(defn scheduled-executor [pool-size thread-name-prefix]
  (->> thread-name-prefix
       thread-factory
       (ScheduledThreadPoolExecutor. pool-size)))

(defonce SCHEDULED-EXECUTOR (scheduled-executor (.availableProcessors (Runtime/getRuntime))
                                                "ZOLO-UTILS-SCHEDULER"))

(defn- safe-fn [thunk descriptor]
  #(try
     (thunk)
     (catch Exception e
       (logger/error e (str "Error periodically executing " descriptor)))))

(defn run-thunk-periodically [descriptor thunk time-period-millis]
  (.scheduleAtFixedRate SCHEDULED-EXECUTOR
                        (safe-fn thunk descriptor)
                        time-period-millis
                        time-period-millis
                        TimeUnit/MILLISECONDS))

