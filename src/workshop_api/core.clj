(ns workshop-api.core
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :refer [response status]]
            [ring.middleware.cors :refer [wrap-cors]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [clojure.string :as str]
            [workshop-api.gemini-describe :as gemini]
            [clojure.core.async :refer [go thread]]
            [cheshire.core :as json]
            [clojure.edn :as edn])
  (:import [java.sql Timestamp]
           [java.time Instant]))

(defn get-db-spec []
  (let [env (System/getenv "DB_ENV")]
    (if (= env "test")
      {:dbtype "postgresql" :dbname "workshop_inventory_test" :host "localhost" :user "billwinkler" :password (System/getenv "DB_PASSWORD")}
      {:dbtype "postgresql" :dbname "workshop_inventory" :host "localhost" :user "billwinkler" :password (System/getenv "DB_PASSWORD")})))

(def ds (jdbc/get-datasource (get-db-spec)))

(defn generate-id [] (str (java.util.UUID/randomUUID)))
(defn current-timestamp [] (Timestamp/from (Instant/now)))

(defn keywordize-keys [m]
  (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) v]) m)))

(defn valid-location? [loc]
  (let [result (and (string? (:name loc))
                    (string? (:type loc))
                    (string? (:area loc))
                    (or (nil? (:label loc)) (string? (:label loc)))
                    (or (nil? (:description loc)) (string? (:description loc)))
                    (or (nil? (:parent_id loc)) (string? (:parent_id loc))))]
    (println "Validating location:" loc "Result:" result)
    result))

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
    (println "Validating item:" item "Result:" result)
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

(defn valid-image? [image]
  (let [result 
        (and (string? (:image_data image))
             (string? (:mime_type image))
             (or (nil? (:filename image)) (string? (:filename image))))]
        (println "Validating image:" image "Result:" result)
        result))

(defn valid-uuid? [s]
  (try
    (java.util.UUID/fromString s)
    true
    (catch IllegalArgumentException _ false)))

(defn valid-analysis-config? [config]
  (let [model-version (if (string? (:model_version config))
                        (keyword (:model_version config))
                        (:model_version config))]
    (println "Validating config:" config)
    (println "Processed model_version:" model-version)
    (println "model_version valid?" (or (nil? model-version) (keyword? model-version)))
    (println "analysis_type valid?" (or (nil? (:analysis_type config)) (string? (:analysis_type config))))
    (and (or (nil? model-version) (keyword? model-version))
         (or (nil? (:analysis_type config)) (string? (:analysis_type config))))))

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

(defn prepare-image [image]
  (let [now (current-timestamp)]
    (-> image
        (assoc :id (generate-id))
        (assoc :status "pending")
        (assoc :created_at now)
        (assoc :updated_at now))))

(defn db-add-location [loc]
  (try
    (jdbc/with-transaction [tx ds]
      (let [result (jdbc/execute-one! tx
                                     ["INSERT INTO locations (id, label, name, type, description, parent_id, area, created_at, updated_at)
                                       VALUES (?::uuid, ?, ?, ?, ?, ?::uuid, ?, ?, ?)"
                                      (:id loc) (:label loc) (:name loc) (:type loc) (:description loc) (:parent_id loc) (:area loc) (:created_at loc) (:updated_at loc)]
                                     {:return-keys true})]
        result))
    (catch Exception e
      (println "Transaction failed:" (.getMessage e))
      (throw e))))

(defn db-add-item [item]
  (jdbc/execute-one! ds
                     ["INSERT INTO items (id, name, category, supplier, supplier_part_no, supplier_item_url, description, notes, quantity, location_id, acquisition_date, created_at, updated_at)
                       VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?)"
                      (:id item) (:name item) (:category item) (:supplier item) (:supplier_part_no item) (:supplier_item_url item) (:description item) (:notes item) (:quantity item) (:location_id item) (:acquisition_date item) (:created_at item) (:updated_at item)]
                     {:return-keys true}))

(defn db-add-image [image]
  (jdbc/execute-one! ds
                     ["INSERT INTO images (id, image_data, mime_type, filename, status, created_at, updated_at)
                       VALUES (?::uuid, ?, ?, ?, ?, ?, ?)"
                      (:id image) (:image_data image) (:mime_type image) (:filename image) (:status image) (:created_at image) (:updated_at image)]
                     {:return-keys true}))

;; added optional conn to support testing
(defn db-update-image [id updates & [conn]]
  (let [ds (or conn ds)
        now (current-timestamp)
        updateable-fields (select-keys updates [:status :gemini_result :error_message])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (if (empty? (dissoc updateable-fields :updated_at))
      (do
        (println "No fields to update for image ID:" id)
        nil)
      (let [sql (str "UPDATE images SET "
                     (str/join ", " (map #(if (= % :gemini_result)
                                            "gemini_result = ?::jsonb"
                                            (str (name %) " = ?"))
                                         (keys updateable-fields)))
                     " WHERE id = ?::uuid")
            params (concat (vals updateable-fields) [id])]
        (jdbc/execute-one! ds
                           (into [sql] params)
                           {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))))
;; added optional conn to support testing
(defn db-get-image [id & [conn]]
  (let [ds (or conn ds)]
    (let [image (jdbc/execute-one! ds
                                   ["SELECT * FROM images WHERE id = ?::uuid" id]
                                   {:builder-fn rs/as-unqualified-lower-maps})]
      (println "db-get-image query for ID:" id "result:" image)
      (if image
        (update image :gemini_result
                #(when % 
                   (try
                     (json/parse-string (.getValue %) true)
                     (catch Exception e
                       (println "Error parsing gemini_result:" (.getMessage e))
                       %))))
        image))))

(defn db-update-item [id item]
  (let [now (current-timestamp)
        updateable-fields (select-keys item [:name :category :supplier :supplier_part_no :supplier_item_url :description :notes :quantity :location_id :acquisition_date])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (if (empty? (dissoc updateable-fields :updated_at))
      (do
        (println "No fields to update for item ID:" id)
        nil)
      (let [sql (str "UPDATE items SET "
                     (str/join ", " (map #(str (name %) " = ?") (keys updateable-fields)))
                     " WHERE id = ?::uuid")
            params (concat (vals updateable-fields) [id])]
        (jdbc/execute-one! ds
                           (into [sql] params)
                           {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))))

(defn db-update-location [id loc]
  (let [now (current-timestamp)
        updateable-fields (select-keys loc [:label :name :type :description :parent_id :area])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (jdbc/execute-one! ds
                       (into ["UPDATE locations SET "
                              (str/join ", " (map #(str (name %) " = ?") (keys updateable-fields)))
                              " WHERE id = ?::uuid"]
                             (concat (vals updateable-fields) [id]))
                       {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))

(defn db-delete-item [id]
  (jdbc/execute-one! ds
                     ["DELETE FROM items WHERE id = ?::uuid" id]))

(defn db-delete-location [id]
  (jdbc/execute-one! ds
                     ["DELETE FROM locations WHERE id = ?::uuid" id]))

(defn db-get-location [id]
  (jdbc/execute-one! ds
                     ["SELECT * FROM locations WHERE id = ?::uuid" id]
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
                 ["SELECT * FROM items WHERE location_id = ?::uuid" location-id]
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-item [id]
  (jdbc/execute-one! ds
                     ["SELECT i.*, l.name AS location_name, l.parent_id
                       FROM items i
                       JOIN locations l ON i.location_id = l.id
                       WHERE i.id = ?::uuid" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-check-location-items [id]
  (jdbc/execute-one! ds
                     ["SELECT COUNT(*) AS count FROM items WHERE location_id = ?::uuid" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-add-item-image [item-id image-id]
  (try
    (jdbc/execute-one! ds
                       ["INSERT INTO item_images (item_id, image_id) VALUES (?::uuid, ?::uuid)"
                        item-id image-id]
                       {:return-keys true})
    (catch Exception e
      (throw (ex-info "Failed to link image to item" {:error (.getMessage e)})))))

(defn db-add-location-image [location-id image-id]
  (try
    (jdbc/execute-one! ds
                       ["INSERT INTO location_images (location_id, image_id) VALUES (?::uuid, ?::uuid)"
                        location-id image-id]
                       {:return-keys true})
    (catch Exception e
      (throw (ex-info "Failed to link image to location" {:error (.getMessage e)})))))

(defn db-delete-item-image [item-id image-id]
  (jdbc/execute-one! ds
                     ["DELETE FROM item_images WHERE item_id = ?::uuid AND image_id = ?::uuid"
                      item-id image-id]))

(defn db-delete-location-image [location-id image-id]
  (jdbc/execute-one! ds
                     ["DELETE FROM location_images WHERE location_id = ?::uuid AND image_id = ?::uuid"
                      location-id image-id]))

(defn db-get-item-images [item-id]
  (jdbc/execute! ds
                 ["SELECT i.*
                   FROM images i
                   JOIN item_images ii ON i.id = ii.image_id
                   WHERE ii.item_id = ?::uuid"
                  item-id]
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-get-location-images [location-id]
  (jdbc/execute! ds
                 ["SELECT i.*
                   FROM images i
                   JOIN location_images li ON i.id = li.image_id
                   WHERE li.location_id = ?::uuid"
                  location-id]
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
    (println "add-location received body:" (:body request))
    (println "add-location keywordized:" loc)
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
  (println "add-item raw request:" (dissoc request :body))
  (println "add-item received body:" (:body request))
  (let [item (keywordize-keys (:body request))]
    (println "add-item keywordized:" item)
    (if (map? (:body request))
      (if (valid-item? item)
        (try
          (let [new-item (prepare-item item)]
            (if (db-get-location (:location_id new-item))
              (do
                (db-add-item new-item)
                (response new-item))
              (status (response {:error "Database error" :message "Invalid location_id"}) 400)))
          (catch Exception e
            (status (response {:error "Database error" :message (.getMessage e)}) 400)))
        (status (response {:error "Invalid item format" :data item}) 400))
      (do
        (println "add-item error: body is not a map, received:" (:body request))
        (status (response {:error "Invalid request body" :data (:body request)}) 400)))))

(defn get-all-items [_request]
  (let [items (db-get-all-items)]
    (response items)))

(defn update-item [request id]
  (let [item (keywordize-keys (:body request))]
    (try
      (if (valid-partial-item? item)
        (if-let [existing-item (db-get-item id)]
          (if-let [updated-item (db-update-item id item)]
            (response updated-item)
            (do
              (println "Failed to update item in database for ID:" id "Item:" item)
              (status (response {:error "No fields to update or failed to update item"}) 400)))
          (status (response {:error "Item not found"}) 404))
        (status (response {:error "Invalid item format" :data item}) 400))
      (catch Exception e
        (println "Error in update-item for ID:" id "Error:" (.getMessage e) "Stacktrace:" (.getStackTrace e))
        (status (response {:error "Internal server error" :message (.getMessage e)}) 500)))))

(defn delete-item [id]
  (if-let [item (db-get-item id)]
    (do
      (db-delete-item id)
      (response {:status "success"}))
    (status (response {:error "Item not found"}) 404)))

(defn get-location-details [id]
  (if-let [loc (db-get-location id)]
    (let [items (db-get-items-by-location id)
          images (db-get-location-images id)]
      (response {:location loc :items items :images images}))
    (status (response {:error "Location not found"}) 404)))

(defn get-all-locations [_request]
  (let [locations (db-get-all-locations)]
    (response locations)))

(defn get-item [id]
  (try
    (if-let [item (db-get-item id)]
      (let [item-with-path (assoc item :location_path (get-location-path (:location_id item)))
            images (db-get-item-images id)]
        (response (assoc item-with-path :images images)))
      (status (response {:error "Item not found"}) 404))
    (catch Exception e
      (println "Error in get-item for ID:" id "Error:" (.getMessage e) "Stacktrace:" (.getStackTrace e))
      (status (response {:error "Internal server error" :message (.getMessage e)}) 500))))

(defn add-item-image [request]
  (let [{:keys [item_id image_id]} (keywordize-keys (:body request))]
    (if (and (valid-uuid? item_id) (valid-uuid? image_id))
      (try
        (if (and (db-get-item item_id) (db-get-image image_id))
          (do
            (db-add-item-image item_id image_id)
            (response {:status "success" :item_id item_id :image_id image_id}))
          (status (response {:error "Item or image not found"}) 404))
        (catch Exception e
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid UUID format" :data {:item_id item_id :image_id image_id}}) 400))))

(defn add-location-image [request]
  (let [{:keys [location_id image_id]} (keywordize-keys (:body request))]
    (if (and (valid-uuid? location_id) (valid-uuid? image_id))
      (try
        (if (and (db-get-location location_id) (db-get-image image_id))
          (do
            (db-add-location-image location_id image_id)
            (response {:status "success" :location_id location_id :image_id image_id}))
          (status (response {:error "Location or image not found"}) 404))
        (catch Exception e
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid UUID format" :data {:location_id location_id :image_id image_id}}) 400))))

(defn delete-item-image [item-id image-id]
  (if (and (valid-uuid? item-id) (valid-uuid? image-id))
    (if (db-get-item item-id)
      (if (db-get-image image-id)
        (do
          (db-delete-item-image item-id image-id)
          (response {:status "success"}))
        (status (response {:error "Image not found"}) 404))
      (status (response {:error "Item not found"}) 404))
    (status (response {:error "Invalid UUID format" :data {:item_id item-id :image_id image-id}}) 400)))

(defn delete-location-image [location-id image-id]
  (if (and (valid-uuid? location-id) (valid-uuid? image-id))
    (if (db-get-location location-id)
      (if (db-get-image image-id)
        (do
          (db-delete-location-image location-id image-id)
          (response {:status "success"}))
        (status (response {:error "Image not found"}) 404))
      (status (response {:error "Location not found"}) 404))
    (status (response {:error "Invalid UUID format" :data {:location_id location-id :image_id image-id}}) 400)))

(defn add-image [request]
  (let [image (keywordize-keys (:body request))]
    (println "add-image received body:" (:body request))
    (println "add-image keywordized:" image)
    (if (valid-image? image)
      (try
        (let [new-image (prepare-image image)]
          (db-add-image new-image)
          (response new-image))
        (catch Exception e
          (println "Error adding image:" (.getMessage e))
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid image format" :data image}) 400))))

(defn analyze-image [request id]
  (println "Analyzing image with ID:" id)
  (println "Raw request body:" (:body request))
  (let [conn (or (:next.jdbc/connection request) ds)]
    (println "Using database:" conn)
    (let [images (jdbc/execute! conn
                                ["SELECT * FROM images"]
                                {:builder-fn rs/as-unqualified-lower-maps})]
      (println "query results:" images))
    (if (valid-uuid? id)
      (if-let [image (db-get-image id conn)]
        (let [config (keywordize-keys (:body request))]
          (println "Processed config:" config)
          (if (valid-analysis-config? config)
            (try
              (println "Starting analysis thread for image:" image)
              (thread
                (try
                  (db-update-image id {:status "processing"} conn)
                  (let [model-version (or (:model_version config) :latest)
                        analysis-type (or (:analysis_type config) "Image analysis")
                        result (gemini/call-gemini-api (:image_data image) model-version analysis-type)]
                    (db-update-image id
                                     {:status "completed"
                                      :gemini_result (json/generate-string result)}
                                     conn))
                  (catch Exception e
                    (println "Error processing image ID:" id "Error:" (.getMessage e))
                    (db-update-image id
                                     {:status "failed"
                                      :error_message (.getMessage e)}
                                     conn))))
              (response {:status "analysis_started" :image_id id}))
            (do
              (println "Invalid analysis config:" config)
              (status (response {:error "Invalid analysis configuration" :data config}) 400))))
        (do
          (println "Image not found for ID:" id)
          (status (response {:error "Image not found"}) 404)))
      (do
        (println "Invalid UUID format:" id)
        (status (response {:error "Invalid UUID format" :id id}) 400)))))

(defn get-image [id]
  (if-let [image (db-get-image id)]
    (response image)
    (status (response {:error "Image not found"}) 404)))

(defn build-location-hierarchy []
  (try
    (let [locations (jdbc/execute! ds
                                   ["SELECT id, label, name, type, description, parent_id, area, created_at, updated_at FROM locations"]
                                   {:builder-fn rs/as-unqualified-lower-maps})
          sanitized-locations (map #(dissoc % :children) locations)
          loc-map (reduce (fn [acc loc] (assoc acc (:id loc) (assoc loc :children []))) {} sanitized-locations)]
      (letfn [(build-node [loc-id visited]
                (if (contains? visited loc-id)
                  (do
                    (println "Circular reference detected at loc-id:" loc-id)
                    {:children []})
                  (let [loc (get loc-map loc-id)]
                    (if-not loc
                      (do
                        (println "Location not found for id:" loc-id)
                        {:children []})
                      (let [children (filter #(= (:parent_id %) loc-id) (vals loc-map))
                            children-with-hierarchy (map #(build-node (:id %) (conj visited loc-id))
                                                         children)]
                        (let [result (assoc loc :children (vec (sort-by :name children-with-hierarchy)))]
                          result))))))]
        (->> (vals loc-map)
             (filter #(nil? (:parent_id %)))
             (map :id)
             (map #(build-node % #{}))
             (map #(select-keys % [:id :label :name :type :description :parent_id :area :children]))
             (sort-by :name)
             vec)))
    (catch Exception e
      (println "Error building location hierarchy:" (.getMessage e))
      (throw (ex-info "Failed to build location hierarchy" {:error (.getMessage e)})))))

(defn get-location-hierarchy [_request]
  (try
    (let [hierarchy (build-location-hierarchy)]
      (response hierarchy))
    (catch Exception e
      (println "Error in get-location-hierarchy:" (.getMessage e))
      (status (response {:error "Internal server error" :message (.getMessage e)}) 500))))

(defn descend-hierarchy
  [hierarchy-or-node path]
  (letfn [(descend [node path]
            (println "Descending at path" path "node type" (type node) "children type" (type (:children node)))
            (cond
              (empty? path) node
              (or (not (map? node)) (not (:children node))) nil
              (not (or (vector? (:children node)) (seq? (:children node))))
              (do
                (println "Warning: :children is not a sequence at path" path "type:" (type (:children node)) "value:" (:children node))
                nil)
              :else (let [index (first path)]
                      (if (and (integer? index) (>= index 0) (< index (count (:children node))))
                        (descend (nth (:children node) index) (rest path))
                        nil))))]
    (if (vector? hierarchy-or-node)
      (if (and (seq path) (integer? (first path)) (>= (first path) 0) (< (first path) (count hierarchy-or-node)))
        (descend (nth hierarchy-or-node (first path)) (rest path))
        nil)
      (descend hierarchy-or-node path))))

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

(defroutes test-routes
  (POST "/test" request
    (response {:received-body (:body request)})))

(defroutes app-routes
  (POST "/api/locations" request (add-location request))
  (PATCH "/api/locations/:id" [id :as request] (update-location request id))
  (DELETE "/api/locations/:id" [id] (delete-location id))
  (GET "/api/locations/hierarchy" request (get-location-hierarchy request))
  (GET "/api/locations/:id" [id] (get-location-details id))
  (POST "/api/items" request (add-item request))
  (PATCH "/api/items/:id" [id :as request] (update-item request id))
  (DELETE "/api/items/:id" [id] (delete-item id))
  (GET "/api/items/:id" [id] (get-item id))
  (GET "/api/search" request (search-inventory request))
  (GET "/api/items" request (get-all-items request))
  (GET "/api/locations" request (get-all-locations request))
  (GET "/api/location/:param" [param] (get-location-by-name-or-label param))
  (POST "/api/images" request (add-image request))
  (GET "/api/images/:id" [id] (get-image id))
  (POST "/api/images/:id/analyze" [id :as request] (analyze-image request id))
  (POST "/api/item-images" request (add-item-image request))
  (DELETE "/api/item-images/:item_id/:image_id" [item-id image-id] (delete-item-image item-id image-id))
  (POST "/api/location-images" request (add-location-image request))
  (DELETE "/api/location-images/:location_id/:image_id" [location-id image-id] (delete-location-image location-id image-id))
  (ANY "*" request
    (println "Unmatched request:" (:uri request))
    (status (response {:error "Route not found" :uri (:uri request)}) 404)))

(defn wrap-log-json-body [handler]
  (fn [request]
    (println "Before wrap-json-body, raw body:" (if (:body request)
                                                   (try
                                                     (slurp (:body request))
                                                     (catch Exception e
                                                       (println "Error reading body:" (.getMessage e))
                                                       "Error"))
                                                   "No body"))
    (let [new-request (handler request)]
      (println "After wrap-json-body, parsed body:" (:body new-request))
      new-request)))

(defn wrap-debug [handler]
  (fn [request]
    (println "Incoming request:" (dissoc request :body))
    (println "Parsed body:" (:body request))
    (handler request)))

(def app
  (-> (routes test-routes app-routes)
;;      (wrap-log-json-body)
      (wrap-json-body {:keywords? true :malformed-response {:status 400 :body "Invalid JSON"}})
      wrap-params
  ;;    wrap-debug
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
