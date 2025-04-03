(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :refer [response status]]))

;; In-memory storage using an atom
(def inventory (atom {}))

;; Helper function to generate unique IDs
(defn generate-id []
  (str (java.util.UUID/randomUUID)))

;; Routes
(defroutes app-routes
  (GET "/hello" [] 
    {:status 200 :body {:message "Hello!"}})
  
  (GET "/inventory" []
    (response @inventory))
  
  (POST "/inventory" request
    (let [item-data (:body request)
          id (generate-id)
          new-item (assoc item-data :id id)]
      (swap! inventory assoc id new-item)
      (response {:status "success" :item new-item})))
  
  (GET "/inventory/:id" [id]
    (if-let [item (get @inventory id)]
      (response item)
      {:status 404 :body {:error "Item not found"}})))

;; Middleware composition
(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (jetty/run-jetty app {:port 3000 :join? false}))
