(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :refer [response status]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.time Instant]
           [java.sql Timestamp]))

;; Database configuration
(def db-spec
  {:dbtype "postgresql"
   :dbname "workshop_inventory"
   :host "localhost"
   :user "billwinkler" 
   :password ""})

;; Initialize datasource
(def ds (jdbc/get-datasource db-spec))

;; Helper functions
(defn generate-id []
  (str (java.util.UUID/randomUUID)))

(defn current-timestamp []
  (Timestamp/from (Instant/now)))  ;; Convert Instant to Timestamp

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

;; Database operations
(defn db-add-item [item]
  (jdbc/execute-one! ds
                     ["INSERT INTO inventory (id, section, shelf, position, category, manufacturer, description, notes, acquisition_date, quantity, created_at, updated_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                      (:id item)
                      (:section item)
                      (:shelf item)
                      (:position item)
                      (:category item)
                      (:manufacturer item)
                      (:description item)
                      (:notes item)
                      (:acquisition-date item)
                      (:quantity item)
                      (:created-at item)
                      (:updated-at item)]
                     {:return-keys true}))

(defn db-get-all-items []
  (jdbc/execute! ds
                 ["SELECT * FROM inventory"]
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-item [id]
  (jdbc/execute-one! ds
                     ["SELECT * FROM inventory WHERE id = ?" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

;; Handlers
(defn add-inventory-item [request]
  (let [item-data (:body request)]
    (if (valid-item? item-data)
      (let [new-item (prepare-item item-data)]
        (db-add-item new-item)
        (response {:status "success" :item new-item}))
      (status (response {:error "Invalid item format"}) 400))))

(defn get-all-inventory []
  (response (db-get-all-items)))

(defn get-inventory-item [id]
  (if-let [item (db-get-item id)]
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
