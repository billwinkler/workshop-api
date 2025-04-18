(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :refer [response status]]
            [ring.middleware.cors :refer [wrap-cors]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [clojure.string :as str])
  (:import [java.sql Timestamp]
           [java.time Instant]))

(def db-spec {:dbtype "postgresql" :dbname "workshop_inventory" :host "localhost" :user "billwinkler" :password (System/getenv "DB_PASSWORD")})
(def ds (jdbc/get-datasource db-spec))

(defn generate-id [] (str (java.util.UUID/randomUUID)))
(defn current-timestamp [] (Timestamp/from (Instant/now)))

(defn keywordize-keys [m]
  (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) v]) m)))

(defn valid-location? [loc]
  (and (string? (:name loc))
       (string? (:type loc))
       (string? (:area loc))
       (or (nil? (:label loc)) (string? (:label loc)))
       (or (nil? (:description loc)) (string? (:description loc)))
       (or (nil? (:parent_id loc)) (string? (:parent_id loc)))))

(defn valid-item? [item]
  (let [name-ok (string? (:name item))
        desc-ok (string? (:description item))
        loc-id-ok (string? (:location_id item))
        cat-ok (or (nil? (:category item)) (string? (:category item)))
        sup-ok (or (nil? (:supplier item)) (string? (:supplier item)))
        part-ok (or (nil? (:supplier_part_no item)) (string? (:supplier_part_no item)))
        url-ok (or (nil? (:supplier_item_url item)) (string? (:supplier_item_url item)))
        qty-ok (or (nil? (:quantity item)) (integer? (:quantity item)))
        notes-ok (or (nil? (:notes item)) (string? (:notes item)))
        acq-date-ok (or (nil? (:acquisition_date item)) (string? (:acquisition_date item)))
        result (and name-ok desc-ok loc-id-ok cat-ok sup-ok part-ok url-ok qty-ok notes-ok acq-date-ok)]
    result))

(defn valid-partial-item? [item]
  (and (or (nil? (:name item)) (string? (:name item)))
       (or (nil? (:description item)) (string? (:description item)))
       (or (nil? (:location_id item)) (string? (:location_id item)))
       (or (nil? (:category item)) (string? (:category item)))
       (or (nil? (:supplier item)) (string? (:supplier item)))
       (or (nil? (:supplier_part_no item)) (string? (:supplier_part_no item)))
       (or (nil? (:supplier_item_url item)) (string? (:supplier_item_url item)))
       (or (nil? (:quantity item)) (integer? (:quantity item)))
       (or (nil? (:notes item)) (string? (:notes item)))
       (or (nil? (:acquisition_date item)) (string? (:acquisition_date item)))))

(defn valid-partial-location? [loc]
  (and (or (nil? (:name loc)) (string? (:name loc)))
       (or (nil? (:type loc)) (string? (:type loc)))
       (or (nil? (:area loc)) (string? (:area loc)))
       (or (nil? (:label loc)) (string? (:label loc)))
       (or (nil? (:description loc)) (string? (:description loc)))
       (or (nil? (:parent_id loc)) (string? (:parent_id loc)))))

(defn prepare-location [loc]
  (let [now (current-timestamp)]
    (-> loc
        (assoc :id (generate-id))
        (assoc :created_at now)
        (assoc :updated_at now))))

(defn prepare-item [item]
  (let [now (current-timestamp)]
    (-> item
        (assoc :id (generate-id))
        (assoc :quantity (or (:quantity item) 1))
        (assoc :created_at now)
        (assoc :updated_at now))))

(defn db-add-location [loc]
  (jdbc/execute-one! ds
                     ["INSERT INTO locations (id, label, name, type, description, parent_id, area, created_at, updated_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                      (:id loc) (:label loc) (:name loc) (:type loc) (:description loc) (:parent_id loc) (:area loc) (:created_at loc) (:updated_at loc)]
                     {:return-keys true}))

(defn db-add-item [item]
  (jdbc/execute-one! ds
                     ["INSERT INTO items (id, name, category, supplier, supplier_part_no, supplier_item_url, description, notes, quantity, location_id, acquisition_date, created_at, updated_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                      (:id item) (:name item) (:category item) (:supplier item) (:supplier_part_no item) (:supplier_item_url item) (:description item) (:notes item) (:quantity item) (:location_id item) (:acquisition_date item) (:created_at item) (:updated_at item)]
                     {:return-keys true}))

(defn db-update-item [id item]
  (let [now (current-timestamp)
        updateable-fields (select-keys item [:name :category :supplier :supplier_part_no :supplier_item_url :description :notes :quantity :location_id :acquisition_date])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (jdbc/execute-one! ds
                       (into ["UPDATE items SET "
                              (str/join ", " (map #(str (name %) " = ?") (keys updateable-fields)))
                              " WHERE id = ?"]
                             (concat (vals updateable-fields) [id]))
                       {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))

(defn db-update-location [id loc]
  (let [now (current-timestamp)
        updateable-fields (select-keys loc [:label :name :type :description :parent_id :area])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (jdbc/execute-one! ds
                       (into ["UPDATE locations SET "
                              (str/join ", " (map #(str (name %) " = ?") (keys updateable-fields)))
                              " WHERE id = ?"]
                             (concat (vals updateable-fields) [id]))
                       {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))

(defn db-delete-item [id]
  (jdbc/execute-one! ds
                     ["DELETE FROM items WHERE id = ?" id]))

(defn db-delete-location [id]
  (jdbc/execute-one! ds
                     ["DELETE FROM locations WHERE id = ?" id]))

(defn db-get-location [id]
  (jdbc/execute-one! ds
                     ["SELECT * FROM locations WHERE id = ?" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-location-by-name [name]
  (jdbc/execute-one! ds
                     ["SELECT * FROM locations WHERE name = ?" name]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-location-by-label [label]
  (jdbc/execute-one! ds
                     ["SELECT * FROM locations WHERE label = ?" label]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-items-by-location [location-id]
  (jdbc/execute! ds
                 ["SELECT * FROM items WHERE location_id = ?" location-id]
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-item [id]
  (jdbc/execute-one! ds
                     ["SELECT i.*, l.name AS location_name, l.parent_id
                       FROM items i
                       JOIN locations l ON i.location_id = l.id
                       WHERE i.id = ?" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-check-location-items [id]
  (jdbc/execute-one! ds
                     ["SELECT COUNT(*) AS count FROM items WHERE location_id = ?" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn get-location-path [loc-id]
  (letfn [(build-path [id acc]
            (if-let [loc (db-get-location id)]
              (if (nil? (:parent_id loc))
                (conj acc (:name loc))
                (build-path (:parent_id loc) (conj acc (:name loc))))
              acc))]
    (str/join " > " (build-path loc-id []))))

(defn db-get-all-items []
  (let [items (jdbc/execute! ds
                            ["SELECT i.*, l.name AS location_name, l.parent_id
                              FROM items i
                              JOIN locations l ON i.location_id = l.id"]
                            {:builder-fn rs/as-unqualified-lower-maps})]
    (map #(assoc % :location_path (get-location-path (:location_id %))) items)))

(defn db-get-all-locations []
  (let [locations (jdbc/execute! ds
                                ["SELECT * FROM locations"]
                                {:builder-fn rs/as-unqualified-lower-maps})]
    (map #(assoc % :location_path (get-location-path (:id %))) locations)))

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
        (response new-loc))
      (status (response {:error "Invalid location format" :data loc}) 400))))

(defn update-location [request id]
  (let [loc (keywordize-keys (:body request))]
    (if (valid-partial-location? loc)
      (if-let [existing-loc (db-get-location id)]
        (if-let [updated-loc (db-update-location id loc)]
          (response updated-loc)
          (status (response {:error "Failed to update location"}) 500))
        (status (response {:error "Location not found"}) 404))
      (status (response {:error "Invalid location format" :data loc}) 400))))

(defn delete-location [id]
  (if-let [loc (db-get-location id)]
    (let [{item-count :count} (db-check-location-items id)]
      (if (zero? item-count)
        (do
          (db-delete-location id)
          (response {:status "success"}))
        (status (response {:error "Cannot delete location with items"}) 400)))
    (status (response {:error "Location not found"}) 404)))

(defn add-item [request]
  (let [item (keywordize-keys (:body request))]
    (if (valid-item? item)
      (try
        (let [new-item (prepare-item item)]
          (db-add-item new-item)
          (response new-item))
        (catch Exception e
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid item format" :data item}) 400))))

(defn update-item [request id]
  (let [item (keywordize-keys (:body request))]
    (if (valid-partial-item? item)
      (if-let [existing-item (db-get-item id)]
        (if-let [updated-item (db-update-item id item)]
          (response updated-item)
          (status (response {:error "Failed to update item"}) 500))
        (status (response {:error "Item not found"}) 404))
      (status (response {:error "Invalid item format" :data item}) 400))))

(defn delete-item [id]
  (if-let [item (db-get-item id)]
    (do
      (db-delete-item id)
      (response {:status "success"}))
    (status (response {:error "Item not found"}) 404)))

(defn get-location-details [id]
  (if-let [loc (db-get-location id)]
    (let [items (db-get-items-by-location id)]
      (response {:location loc :items items}))
    (status (response {:error "Location not found"}) 404)))

(defn get-item [id]
  (try
    (if-let [item (db-get-item id)]
      (let [item-with-path (assoc item :location_path (get-location-path (:location_id item)))]
        (response item-with-path))
      (status (response {:error "Item not found"}) 404))
    (catch Exception e
      (println "Error in get-item for ID:" id "Error:" (.getMessage e) "Stacktrace:" (.getStackTrace e))
      (status (response {:error "Internal server error" :message (.getMessage e)}) 500))))

(defn get-all-items [_request]
  (let [items (db-get-all-items)]
    (response items)))

(defn get-all-locations [_request]
  (let [locations (db-get-all-locations)]
    (response locations)))

(defn get-location-by-name-or-label [param]
  (let [normalized-param (str/replace param "+" " ")
        url-pattern (re-pattern "/locations/([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})$")
        uuid-pattern (re-pattern "^([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})$")
        url-match (re-find url-pattern normalized-param)
        uuid-match (re-find uuid-pattern normalized-param)
        id (or (when url-match (second url-match))
               (when uuid-match (second uuid-match)))]
    (println "Fetching location by name or label:" normalized-param)
    (try
      (or
       (when id
         (if-let [loc (db-get-location id)]
           (let [loc-with-path (assoc loc :location_path (get-location-path (:id loc)))]
             (println "Found location by ID:" loc-with-path)
             (response loc-with-path))
           (do
             (println "Location not found for ID:" id)
             (status (response {:error "Location not found" :id id}) 404))))
       (when-let [loc (db-get-location-by-name normalized-param)]
         (let [loc-with-path (assoc loc :location_path (get-location-path (:id loc)))]
           (println "Found location by name:" loc-with-path)
           (response loc-with-path)))
       (when-let [loc (db-get-location-by-label normalized-param)]
         (let [loc-with-path (assoc loc :location_path (get-location-path (:id loc)))]
           (println "Found location by label:" loc-with-path)
           (response loc-with-path)))
       (do
         (println "Location not found for name or label:" normalized-param)
         (status (response {:error "Location not found"}) 404)))
      (catch Exception e
        (println "Error fetching location:" (.getMessage e))
        (status (response {:error "Internal server error" :message (.getMessage e)}) 500)))))

(defn search-inventory [request]
  (let [query (get (:query-params request) "q")
        items (if (str/blank? query)
                []
                (db-search-items query))]
    (response items)))

(defroutes app-routes
  (POST "/api/locations" request (add-location request))
  (PATCH "/api/locations/:id" [id :as request] (update-location request id))
  (DELETE "/api/locations/:id" [id] (delete-location id))
  (GET "/api/locations/:id" [id] (get-location-details id))
  (POST "/api/items" request (add-item request))
  (PATCH "/api/items/:id" [id :as request] (update-item request id))
  (DELETE "/api/items/:id" [id] (delete-item id))
  (GET "/api/items/:id" [id] (get-item id))
  (GET "/api/search" request (search-inventory request))
  (GET "/api/items" request (get-all-items request))
  (GET "/api/locations" request (get-all-locations request))
  (GET "/api/location/:param" [param] (get-location-by-name-or-label param)))

(def app
  (-> app-routes
      wrap-params
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :patch :delete])))

(defn test-connection []
  (try
    (jdbc/execute-one! ds ["SELECT 1"])
    (println "Database connection successful")
    (catch Exception e
      (println "Database connection failed:" (.getMessage e)))))

(defn -main []
  (println "Starting workshop-api v0.1.0")
  (test-connection)
  (jetty/run-jetty app {:port 3000 :join? false}))
