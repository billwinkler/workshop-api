(ns workshop-api.middleware
  (:require [buddy.auth :refer [authenticated? throw-unauthorized]]
            [taoensso.timbre :as log]))

(defn wrap-log-json-body [handler]
  (fn [request]
    (log/debug "Before wrap-json-body, raw body:" (if (:body request)
                                                   (try
                                                     (slurp (:body request))
                                                     (catch Exception e
                                                       (log/debug "Error reading body:" (.getMessage e))
                                                       "Error"))
                                                   "No body"))
    (let [new-request (handler request)]
      (log/debug "After wrap-json-body, parsed body:" (:body new-request))
      new-request)))

(defn wrap-debug [handler]
  (fn [request]
    (log/debug "Incoming request:" (dissoc request :body))
    (log/debug "Parsed body:" (:body request))
    (handler request)))

(defn wrap-auth [handler]
  (fn [request]
    (if (authenticated? request)
      (handler request)
      (do (log/debug "in wrap-auth, throwing unauthorized:")
          (throw-unauthorized {:message "Unauthorized"})))))

(defn wrap-error-handling [handler]
  (fn [request]
    (log/debug "in wrap-error-handling with request:" request)
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        (log/error "wrap-error-handling caught ExceptionInfo:" (ex-data e))
        (if (= (:buddy.auth/type (ex-data e)) :buddy.auth/unauthorized)
          {:status 401 :body {:message "Unauthorized"}}
          (do
            (log/error "Unexpected ExceptionInfo:" (.getMessage e))
            {:status 500 :body {:error "Internal server error" :message (.getMessage e)}})))
      (catch Exception e
        (log/error "wrap-error-handling caught Exception:" (.getMessage e) "Stacktrace:" (.getStackTrace e))
        {:status 500 :body {:error "Internal server error" :message (.getMessage e)}}))))

