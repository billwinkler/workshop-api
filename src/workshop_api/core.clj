(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.params :refer [wrap-params]]  ;; Added
            [ring.util.response :refer [response status]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [clojure.string :as str])
  (:import [java.sql Timestamp]
           [java.time Instant]))

(def db-spec {:dbtype "postgresql" :dbname "workshop_inventory" :host "localhost" :user "billwinkler" :password ""})
(def ds (jdbc/get-datasource db-spec))

(defn generate-id [] (str (java.util.UUID/randomUUID)))
(defn current-timestamp [] (Timestamp/from (Instant/now)))

(defn keywordize-keys [m]
  (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) v]) m)))

(defn valid-location? [loc]
  (and (string? (:name loc))
       (string? (:type loc))
       (string? (:area loc))
       (or (nil? (:description loc)) (string? (:description loc)))
       (or (nil? (:parent_id loc)) (string? (:parent_id loc)))))

(defn valid-item? [item]
  (let [name-ok (string? (:name item))
        desc-ok (string? (:description item))
        loc-id-ok (string? (:location_id item))
        cat-ok (or (nil? (:category item)) (string? (:category item)))
        sup-ok (or (nil? (:supplier item)) (string? (:supplier item)))
        qty-ok (or (nil? (:quantity item)) (integer? (:quantity item)))
        notes-ok (or (nil? (:notes item)) (string? (:notes item)))
        acq-date-ok (or (nil? (:acquisition_date item)) (string? (:acquisition_date item)))
        result (and name-ok desc-ok loc-id-ok cat-ok sup-ok qty-ok notes-ok acq-date-ok)]
    result))

(defn prepare-location [loc]
  (let [now (current-timestamp)]
    (-> loc
        (assoc :id (generate-id))
        (assoc :created_at now)  ;; Changed to underscore
        (assoc :updated_at now))))  ;; Changed to underscore

(defn prepare-item [item]
  (let [now (current-timestamp)]
    (-> item
        (assoc :id (generate-id))
        (assoc :quantity (or (:quantity item) 1))
        (assoc :created_at now)  ;; Changed to underscore
        (assoc :updated_at now))))  ;; Changed to underscore

(defn db-add-location [loc]
  (jdbc/execute-one! ds
                     ["INSERT INTO locations (id, name, description, type, parent_id, area, created_at, updated_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                      (:id loc) (:name loc) (:description loc) (:type loc) (:parent_id loc) (:area loc) (:created_at loc) (:updated_at loc)]
                     {:return-keys true}))

(defn db-add-item [item]
  (jdbc/execute-one! ds
                     ["INSERT INTO items (id, name, category, supplier, description, notes, quantity, location_id, acquisition_date, created_at, updated_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                      (:id item) (:name item) (:category item) (:supplier item) (:description item) (:notes item) (:quantity item) (:location_id item) (:acquisition_date item) (:created_at item) (:updated_at item)]
                     {:return-keys true}))

(defn db-get-location [id]
  (jdbc/execute-one! ds
                     ["SELECT * FROM locations WHERE id = ?" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-items-by-location [location-id]
  (jdbc/execute! ds
                 ["SELECT * FROM items WHERE location_id = ?" location-id]
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn get-location-path [loc-id]
  (letfn [(build-path [id acc]
            (if-let [loc (db-get-location id)]
              (if (nil? (:parent_id loc))
                (conj acc (:name loc))
                (build-path (:parent_id loc) (conj acc (:name loc))))
              acc))]
    (str/join " > " (build-path loc-id []))))

(defn db-search-items [query]
  (let [search-term (str "%" query "%")
        items (jdbc/execute! ds
                            ["SELECT i.*, l.name AS location_name, l.parent_id
                              FROM items i
                              JOIN locations l ON i.location_id = l.id
                              WHERE i.name ILIKE ? OR i.category ILIKE ? OR i.supplier ILIKE ? OR i.description ILIKE ? OR i.notes ILIKE ?"
                             search-term search-term search-term search-term search-term]
                            {:builder-fn rs/as-unqualified-lower-maps})]
    (map #(assoc % :location_path (get-location-path (:location_id %))) items)))

(defn add-location [request]
  (let [loc (keywordize-keys (:body request))]
    (if (valid-location? loc)
      (let [new-loc (prepare-location loc)]
        (db-add-location new-loc)
        (response {:status "success" :location new-loc}))
      (status (response {:error "Invalid location format" :data loc}) 400))))

(defn add-item [request]
  (let [item (keywordize-keys (:body request))]
    (if (valid-item? item)
      (try
        (let [new-item (prepare-item item)]
          (db-add-item new-item)
          (response {:status "success" :item new-item}))
        (catch Exception e
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid item format" :data item}) 400))))

(defn get-location-details [id]
  (if-let [loc (db-get-location id)]
    (let [items (db-get-items-by-location id)]
      (response {:location loc :items items}))
    (status (response {:error "Location not found"}) 404)))

;; New search handler
(defn search-inventory [request]
  (let [query (get (:query-params request) "q")
        items (if (str/blank? query)
                []
                (db-search-items query))]
    (response items)))

(defroutes app-routes
  (POST "/location" request (add-location request))
  (POST "/item" request (add-item request))
  (GET "/inventory/location/:id" [id] (get-location-details id))
  (GET "/inventory/search" request (search-inventory request))) ;; a new route

(def app
  (-> app-routes
      wrap-params  ;; Added wrap-params
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (jetty/run-jetty app {:port 3000 :join? false}))
