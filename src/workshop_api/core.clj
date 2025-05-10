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
            [workshop-api.utils.git :refer [git-describe-tags]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
            [taoensso.timbre :as log]))

(log/merge-config! {:min-level :error})

(log/with-merged-config {:min-level :debug}
  (log/info {:db_env (System/getenv "DB_ENV") :git-version (git-describe-tags)}))

(def app
  (-> (routes
       routes/test-routes
       routes/auth-routes
       routes/app-routes)
      (wrap-json-body {:keywords? true :malformed-response {:status 400 :body "Invalid JSON"}})
      wrap-params
      middleware/wrap-error-handling
      (wrap-authentication auth/auth-backend)
      (wrap-authorization auth/auth-backend)
      wrap-json-response
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :patch :delete])))

(defn test-connection []
  (try
    (jdbc/execute-one! db/ds ["SELECT 1"])
    (log/debug "Database connection successful")
    (catch Exception e
      (log/debug "Database connection failed:" (.getMessage e)))))

(defn -main []
  (log/info "Starting workshop-api v0.1.0")
  (test-connection)
  (jetty/run-jetty app {:port 3000 :join? false}))
