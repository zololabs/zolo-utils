(ns zolo.utils.system)

(defn system-env [property]
  (.get (System/getenv) property))
