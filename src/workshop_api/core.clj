(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :refer [response status]]
            [clj-time.core :as t]
            [clj-time.format :as f]))

;; In-memory storage using an atom
(def inventory (atom {}))

;; Date formatter for timestamps
(def iso-formatter (f/formatters :date-time))

;; Helper functions
(defn generate-id []
  (str (java.util.UUID/randomUUID)))

(defn current-timestamp []
  (f/unparse iso-formatter (t/now)))

;; Validation function
(defn valid-item? [item]
  (and (string? (:section item))
       (integer? (:shelf item))
       (integer? (:position item))
       (string? (:category item))
       (string? (:manufacturer item))
       (string? (:description item))
       (or (nil? (:quantity item)) (integer? (:quantity item)))
       (or (nil? (:notes item)) (string? (:notes item)))
       (or (nil? (:acquisition-date item)) (string? (:acquisition-date item)))))

;; Prepare item with defaults and timestamps
(defn prepare-item [item]
  (let [now (current-timestamp)]
    (-> item
        (assoc :id (generate-id))
        (assoc :quantity (or (:quantity item) 1))
        (assoc :created-at now)
        (assoc :updated-at now))))

;; Handlers
(defn add-inventory-item [request]
  (let [item-data (:body request)]
    (if (valid-item? item-data)
      (let [new-item (prepare-item item-data)]
        (swap! inventory assoc (:id new-item) new-item)
        (response {:status "success" :item new-item}))
      (status (response {:error "Invalid item format"}) 400))))

(defn get-all-inventory []
  (response (vals @inventory)))

(defn get-inventory-item [id]
  (if-let [item (get @inventory id)]
    (response item)
    (status (response {:error "Item not found"}) 404)))

;; Routes
(defroutes app-routes
  (GET "/hello" [] 
    {:status 200 :body {:message "Hello!"}})
  
  (GET "/inventory" []
    (get-all-inventory))
  
  (POST "/inventory" request
    (add-inventory-item request))
  
  (GET "/inventory/:id" [id]
    (get-inventory-item id)))

;; Middleware composition
(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (jetty/run-jetty app {:port 3000 :join? false}))
