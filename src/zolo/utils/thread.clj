(ns zolo.utils.thread
  (:use zolo.utils.clojure)
  (:require [zolo.utils.logger :as logger])
  (:import [java.util.concurrent ThreadFactory ScheduledThreadPoolExecutor TimeUnit]))

(def CPU-COUNT (.availableProcessors (Runtime/getRuntime)))

(def SCHEDULED-EXECUTOR (atom nil))

(defn- random-thread-name [prefix]
  (str prefix "-" (random-guid-str)))

(defn thread-factory [thread-name-prefix]
  (proxy [ThreadFactory] []
    (newThread [thunk]
      (Thread. thunk (random-thread-name thread-name-prefix)))))

(defn- scheduled-thread-pool-executor [pool-size thread-name-prefix]
  (->> thread-name-prefix
       thread-factory
       (ScheduledThreadPoolExecutor. pool-size)))

(defn- init-scheduled-executor []
  (if (nil? @SCHEDULED-EXECUTOR)
    (reset! SCHEDULED-EXECUTOR (scheduled-thread-pool-executor CPU-COUNT "ZOLO-UTILS-SCHEDULER"))))

(defn protected [thunk descriptor]
  #(try
     (thunk)
     (catch Exception e
       (logger/error e (str "Error periodically executing " descriptor)))))

(defn run-thunk-periodically [descriptor thunk time-period-millis]
  (init-scheduled-executor)
  (.scheduleAtFixedRate @SCHEDULED-EXECUTOR (protected thunk descriptor) time-period-millis time-period-millis TimeUnit/MILLISECONDS))

