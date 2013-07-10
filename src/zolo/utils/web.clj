(ns zolo.utils.web
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [clojure.data.json :as json]
            [zolo.utils.maps :as zmaps]
            [zolo.utils.logger :as logger]
            [zolo.utils.http-status-codes :as http-status]))

(def ^:dynamic *WEB-REQUEST*)

(defn request-origin []
  (get-in *WEB-REQUEST* [:headers "origin"]))

(defn- jsonify [response-map]
  (-> {:headers (merge {"Content-Type" "application/json; charset=utf-8"
                        "Access-Control-Allow-Origin" (request-origin)
                        "Access-Control-Allow-Credentials" "true"}
                       (:headers response-map))}
      (assoc :body (json/json-str (zmaps/to-underscore-keys (:body response-map))))
      (assoc :status (:status response-map))))

(defn- json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json; charset=utf-8"
             "Access-Control-Allow-Origin" (request-origin)
             "Access-Control-Allow-Credentials" "true"
             "Cache-Control:" "max-age=0, no-cache,  must-revalidate"}
   :body (json/json-str (zmaps/to-underscore-keys data))})

(defn- error-response [error-object]
  (json-response {:error (:message error-object)}
                 (http-status/STATUS-CODES (:type error-object))))

(defn wrap-jsonify [handler]
  (fn [request]
    (-> (handler request)
        jsonify)))

(defn wrap-error-handling [handler]
  (fn [request]
    (try+
     (handler request)
     (catch [:type :bad-request] e
       (logger/error e "Bad Request :")
       (error-response e))
     (catch [:type :not-found] e
       (logger/error e "Not found :")
       (error-response e))
     (catch [:type :forbidden] e
       (logger/error e "Permission denied :")
       (error-response e))
     (catch Exception e
       (logger/error e "Exception Occured :")
       (json-response {:error (.getMessage e)} 500)))))

(defn wrap-request-logging [dont-log-req-check-fn logging-context-req-fn handler]
  (fn [request]
    (if (dont-log-req-check-fn request)
      (logger/with-logging-context (logging-context-req-fn request)
        (logger/debug "REQUEST : " request)
        (let [response (handler request)]
          (logger/debug "RESPONSE : " (assoc response :body "FILTERED"))
          response))
      (handler request))))

(defn wrap-request-binding [handler]
  (fn [request]
    (binding [*WEB-REQUEST* request]
      (handler request))))