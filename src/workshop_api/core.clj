(ns workshop-api.core
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-response]]))

(defroutes app-routes
  (GET "/hello" [] {:status 200 :body {:message "Hello from Clojure!"}}))

(def app
  (-> app-routes
      wrap-json-response))

(defn -main []
  (jetty/run-jetty app {:port 3000 :join? false}))
