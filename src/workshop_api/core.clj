(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :refer [response status]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.sql Timestamp]
           [java.time Instant]))

(def db-spec {:dbtype "postgresql" :dbname "workshop_inventory" :host "localhost" :user "billwinkler" :password ""})
(def ds (jdbc/get-datasource db-spec))

(defn generate-id [] (str (java.util.UUID/randomUUID)))
(defn current-timestamp [] (Timestamp/from (Instant/now)))

;; Validation
(defn valid-location? [loc]
  (and (string? (:name loc))
       (string? (:type loc))
       (string? (:area loc))
       (or (nil? (:description loc)) (string? (:description loc)))
       (or (nil? (:parent-id loc)) (string? (:parent-id loc)))))

(defn valid-item? [item]
  (and (string? (:name item))
       (string? (:description item))
       (string? (:location-id item))
       (or (nil? (:category item)) (string? (:category item)))
       (or (nil? (:supplier item)) (string? (:supplier item)))
       (or (nil? (:quantity item)) (integer? (:quantity item)))
       (or (nil? (:notes item)) (string? (:notes item)))
       (or (nil? (:acquisition-date item)) (string? (:acquisition-date item)))))

;; Prepare entities
(defn prepare-location [loc]
  (let [now (current-timestamp)]
    (-> loc
        (assoc :id (generate-id))
        (assoc :created-at now)
        (assoc :updated-at now))))

(defn prepare-item [item]
  (let [now (current-timestamp)]
    (-> item
        (assoc :id (generate-id))
        (assoc :quantity (or (:quantity item) 1))
        (assoc :created-at now)
        (assoc :updated-at now))))

;; Database operations
(defn db-add-location [loc]
  (jdbc/execute-one! ds
                     ["INSERT INTO locations (id, name, description, type, parent_id, area, created_at, updated_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?)"  ;; Fixed typo: removed extra ?, ?
                      (:id loc) (:name loc) (:description loc) (:type loc) (:parent-id loc) (:area loc) (:created-at loc) (:updated-at loc)]
                     {:return-keys true}))

(defn db-add-item [item]
  (jdbc/execute-one! ds
                     ["INSERT INTO items (id, name, category, supplier, description, notes, quantity, location_id, acquisition_date, created_at, updated_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                      (:id item) (:name item) (:category item) (:supplier item) (:description item) (:notes item) (:quantity item) (:location-id item) (:acquisition-date item) (:created-at item) (:updated-at item)]
                     {:return-keys true}))

(defn db-get-location [id]
  (jdbc/execute-one! ds ["SELECT * FROM locations WHERE id = ?" id] {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-items-by-location [location-id]
  (jdbc/execute! ds ["SELECT * FROM items WHERE location_id = ?" location-id] {:builder-fn rs/as-unqualified-lower-maps}))

;; Handlers
(defn add-location [request]
  (let [loc (:body request)]
    (if (valid-location? loc)
      (let [new-loc (prepare-location loc)]
        (db-add-location new-loc)
        (response {:status "success" :location new-loc}))
      (status (response {:error "Invalid location format"}) 400))))

(defn add-item [request]
  (let [item (:body request)]
    (if (valid-item? item)
      (let [new-item (prepare-item item)]
        (db-add-item new-item)
        (response {:status "success" :item new-item}))
      (status (response {:error "Invalid item format"}) 400))))

(defn get-location-details [id]
  (if-let [loc (db-get-location id)]
    (let [items (db-get-items-by-location id)]
      (response {:location loc :items items}))
    (status (response {:error "Location not found"}) 404)))

;; Simple hello endpoint
(defn hello []
  (response {:message "Hello, Workshop API!"}))

;; Routes
(defroutes app-routes
  (GET "/hello" [] (hello))  ;; Added this route
  (POST "/location" request (add-location request))
  (POST "/item" request (add-item request))
  (GET "/inventory/location/:id" [id] (get-location-details id)))

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (jetty/run-jetty app {:port 3000 :join? false}))
