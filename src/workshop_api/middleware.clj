(ns workshop-api.middleware
  (:require [buddy.auth :refer [authenticated? throw-unauthorized]]))

(defn wrap-log-json-body [handler]
  (fn [request]
    (println "Before wrap-json-body, raw body:" (if (:body request)
                                                   (try
                                                     (slurp (:body request))
                                                     (catch Exception e
                                                       (println "Error reading body:" (.getMessage e))
                                                       "Error"))
                                                   "No body"))
    (let [new-request (handler request)]
      (println "After wrap-json-body, parsed body:" (:body new-request))
      new-request)))

(defn wrap-debug [handler]
  (fn [request]
    (println "Incoming request:" (dissoc request :body))
    (println "Parsed body:" (:body request))
    (handler request)))

(defn wrap-auth [handler]
  (fn [request]
    (if (authenticated? request)
      (handler request)
      (do (println "in wrap-auth, throwing unauthorized:")
          (throw-unauthorized {:message "Unauthorized"})))))

(defn wrap-error-handling [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        (println "wrap-error-handling caught ExceptionInfo:" (ex-data e))
        (if (= (:buddy.auth/type (ex-data e)) :buddy.auth/unauthorized)
          {:status 401 :body {:message "Unauthorized"}}
          (do
            (println "Unexpected ExceptionInfo:" (.getMessage e))
            {:status 500 :body {:error "Internal server error" :message (.getMessage e)}})))
      (catch Exception e
        (println "wrap-error-handling caught Exception:" (.getMessage e) "Stacktrace:" (.getStackTrace e))
        {:status 500 :body {:error "Internal server error" :message (.getMessage e)}}))))

