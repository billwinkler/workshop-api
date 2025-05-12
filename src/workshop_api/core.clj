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
            [workshop-api.utils.git :refer [git-describe-tags git-commit-hash]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
            [taoensso.timbre :as log]))

(log/merge-config! {:min-level :debug})
(log/debug "is debug working?")

(log/with-merged-config {:min-level :debug}
  (log/report "TIMBRE_MIN_LEVEL:" (System/getenv "TIMBRE_MIN_LEVEL"))
  (log/report "DB_ENV:" (System/getenv "DB_ENV"))
  (log/report "GIT-VERSION:" (let [tag (git-describe-tags)]
                               (if (= tag "Git hash not found")
                                 (git-commit-hash)
                                 tag))))

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
  (log/info "Starting workshop-api v0.1.2")
  (test-connection)
  (jetty/run-jetty app {:port 3000
                        :max-form-body-size (* 2 1024 1024) ; 2 MB
                        :join? false}))
