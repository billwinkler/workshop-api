(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.cors :refer [wrap-cors]]
            [compojure.core :refer [routes]]
            [next.jdbc :as jdbc]
            [workshop-api.db :as db]
            [workshop-api.auth :as auth]
            [workshop-api.routes :as routes]
            [workshop-api.middleware :as middleware]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]))

(defn debug-wrap-params-v0 [handler]
  (fn [request]
    (println "wrap-params input request:" (select-keys request [:query-string :query-params]))
    (let [response (wrap-params handler)]
      (println "wrap-params output query-params:" (:query-params (response request)))
      (response request))))

(defn debug-wrap-params [handler]
  (fn [request]
    (println "wrap-params input request:" (select-keys request [:query-string :query-params]))
    (let [wrapped-handler (wrap-params handler)]
      (let [response (wrapped-handler request)]
        (println "wrap-params output query-params:" (:query-params request))
        response))))

(defn wrap-error-handling [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        (println "wrap-error-handling caught ExceptionInfo:" (ex-data e) "URI:" (:uri request))
        (if (= (:buddy.auth/type (ex-data e)) :buddy.auth/unauthorized)
          {:status 401 :body {:message "Unauthorized"}}
          (do
            (println "Unexpected ExceptionInfo:" (.getMessage e))
            {:status 500 :body {:error "Internal server error" :message (.getMessage e)}})))
      (catch Exception e
        (println "wrap-error-handling caught Exception:" (.getMessage e) "Stacktrace:" (.getStackTrace e) "URI:" (:uri request))
        {:status 500 :body {:error "Internal server error" :message (.getMessage e)}}))))

(def app
  (-> (routes
       routes/test-routes
       routes/auth-routes
       routes/app-routes)
      (wrap-json-body {:keywords? true :malformed-response {:status 400 :body "Invalid JSON"}})
      debug-wrap-params
      middleware/wrap-error-handling
      (wrap-authentication auth/auth-backend)
      (wrap-authorization auth/auth-backend)
      wrap-json-response
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :patch :delete])))

(defn test-connection []
  (try
    (jdbc/execute-one! db/ds ["SELECT 1"])
    (println "Database connection successful")
    (catch Exception e
      (println "Database connection failed:" (.getMessage e)))))

(defn -main []
  (println "Starting workshop-api v0.1.0")
  (test-connection)
  (jetty/run-jetty app {:port 3000 :join? false}))
