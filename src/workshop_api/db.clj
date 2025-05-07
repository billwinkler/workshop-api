(ns workshop-api.db
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [clojure.string :as str]
            [clojure.set :as set]
            [cheshire.core :as json]
            [workshop-api.common :refer [valid-uuid?]])
  (:import [java.sql Timestamp]
           [java.time Instant]))

(defn get-db-spec []
  (let [env (System/getenv "DB_ENV")]
    (if (= env "test")
      {:dbtype "postgresql" :dbname "workshop_inventory_test" :host "localhost" :user "billwinkler" :password (System/getenv "DB_PASSWORD")}
      {:dbtype "postgresql" :dbname "workshop_inventory" :host "localhost" :user "billwinkler" :password (System/getenv "DB_PASSWORD")})))

;;(def ds (jdbc/get-datasource {:dbtype "postgresql" :dbname "workshop_inventory" :host "localhost" :user "billwinkler" :password (System/getenv "DB_PASSWORD")}))

(def ds (jdbc/get-datasource (get-db-spec)))

(defn current-timestamp [] (Timestamp/from (Instant/now)))

(defn keywordize-keys [m]
  (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) v]) m)))

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

(defn db-add-image-analysis [analysis & [conn]]
  (let [ds (or conn ds)]
    (jdbc/execute-one! ds
                       ["INSERT INTO image_analyses (id, image_id, status, model_version, analysis_type, result, error_message, created_at, updated_at)
                         VALUES (?::uuid, ?::uuid, ?, ?, ?, ?::jsonb, ?, ?, ?)"
                        (:id analysis) (:image_id analysis) (:status analysis) (:model_version analysis) (:analysis_type analysis) (:result analysis) (:error_message analysis) (:created_at analysis) (:updated_at analysis)]
                       {:return-keys true})))

(defn db-update-image-analysis [id updates & [conn]]
  (let [ds (or conn ds)
        now (current-timestamp)
        updateable-fields (select-keys updates [:status :result :error_message :model_version :analysis_type])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (if (empty? (dissoc updateable-fields :updated_at))
      (do
        (println "No fields to update for image analysis ID:" id)
        nil)
      (let [sql (str "UPDATE image_analyses SET "
                     (str/join ", " (map #(if (= % :result)
                                            "result = ?::jsonb"
                                            (str (name %) " = ?"))
                                         (keys updateable-fields)))
                     " WHERE id = ?::uuid")
            params (concat (vals updateable-fields) [id])]
        (jdbc/execute-one! ds
                           (into [sql] params)
                           {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))))

(defn db-get-image-analyses [image-id & [conn]]
  (let [ds (or conn ds)]
    (let [analyses (jdbc/execute! ds
                                  ["SELECT * FROM image_analyses WHERE image_id = ?::uuid ORDER BY created_at DESC" image-id]
                                  {:builder-fn rs/as-unqualified-lower-maps})]
      (map #(update % :result
                    (fn [result]
                      (when result
                        (try
                          (json/parse-string (.getValue result) true)
                          (catch Exception e
                            (println "Error parsing result:" (.getMessage e))
                            result)))))
           analyses))))

(defn db-get-image-analysis-by-id [analysis-id & [conn]]
  (let [ds (or conn ds)]
    (let [analysis (jdbc/execute-one! ds
                                      ["SELECT * FROM image_analyses WHERE id = ?::uuid" analysis-id]
                                      {:builder-fn rs/as-unqualified-lower-maps})]
      (if analysis
        (update analysis :result
                (fn [result]
                  (when result
                    (try
                      (json/parse-string (.getValue result) true)
                      (catch Exception e
                        (println "Error parsing result:" (.getMessage e))
                        result)))))
        nil))))

(defn db-update-image [id updates & [conn]]
  (let [ds (or conn ds)
        now (current-timestamp)
        updateable-fields (select-keys updates [:status :filename :mime_type :image_data])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (if (empty? (dissoc updateable-fields :updated_at))
      (do
        (println "No fields to update for image ID:" id)
        nil)
      (let [sql (str "UPDATE images SET "
                     (str/join ", " (map #(str (name %) " = ?") (keys updateable-fields)))
                     " WHERE id = ?::uuid")
            params (concat (vals updateable-fields) [id])]
        (jdbc/execute-one! ds
                           (into [sql] params)
                           {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))))

(defn db-get-image [id & [conn]]
  (let [ds (or conn ds)]
    (jdbc/execute-one! ds
                       ["SELECT * FROM images WHERE id = ?::uuid" id]
                       {:builder-fn rs/as-unqualified-lower-maps})))

(defn db-get-images
  ([fields]
   (db-get-images fields nil))
  ([fields conn]
   (let [ds (or conn ds)
         allowed-fields #{"id" "image_data" "mime_type" "filename" "status" "created_at" "updated_at"}
         valid-fields (set/intersection fields allowed-fields)
         selected-fields (if (empty? valid-fields)
                           #{"id" "filename" "mime_type" "status"}
                           valid-fields)
         sql (str "SELECT " (str/join ", " (map name selected-fields)) " FROM images")]
     (jdbc/execute! ds
                    [sql]
                    {:builder-fn rs/as-unqualified-lower-maps}))))

(defn db-add-user [user]
  (try
    (jdbc/execute-one! ds
                       ["INSERT INTO users (id, username, password_hash, created_at, updated_at)
                         VALUES (?::uuid, ?, ?, ?, ?)"
                        (:id user) (:username user) (:password_hash user) (:created_at user) (:updated_at user)]
                       {:return-keys true :builder-fn rs/as-unqualified-lower-maps})
    (catch Exception e
      (println "Failed to add user:" (.getMessage e))
      (throw e))))

(defn db-get-user-by-username [username]
  (jdbc/execute-one! ds
                     ["SELECT * FROM users WHERE username = ?" username]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn db-update-item [id item]
  (let [now (current-timestamp)
        updateable-fields (select-keys item [:name :category :supplier :supplier_part_no
                                             :supplier_item_url :description :notes :quantity
                                             :location_id :acquisition_date])
        updateable-fields (assoc updateable-fields :updated_at now)]
        (println "### Attempting to update item ID:" id "with fields:" updateable-fields)
        (println "### Valid location_id:" (valid-uuid? (:location_id updateable-fields)))
        (if (empty? (dissoc updateable-fields :updated_at))
          (do
            (println "### No fields to update for item ID:" id)
            nil)
          (let [sql (str "UPDATE items SET "
                         (str/join ", " (map #(str (name %) " = ?") (keys updateable-fields)))
                         " WHERE id = ?::uuid")
                params (concat (vals updateable-fields) [id])]
            (jdbc/execute-one! ds
                               (into [sql] params)
                               {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))))

(defn db-update-item [id item]
  (let [now (current-timestamp)
        updateable-fields (select-keys item [:name :category :supplier :supplier_part_no
                                             :supplier_item_url :description :notes :quantity
                                             :location_id :acquisition_date])
        updateable-fields (assoc updateable-fields :updated_at now)]
    (println "### Attempting to update item ID:" id "with fields:" updateable-fields)
    (println "### Valid location_id:" (valid-uuid? (:location_id updateable-fields)))
    (if (empty? (dissoc updateable-fields :updated_at))
      (do
        (println "### No fields to update for item ID:" id)
        nil)
      (let [sql (str "UPDATE items SET "
                     (str/join ", " (map (fn [k]
                                           (if (= k :location_id)
                                             "location_id = ?::uuid"
                                             (str (name k) " = ?")))
                                         (keys updateable-fields)))
                     " WHERE id = ?::uuid")
            _ (println "### sql: " sql)
            params (concat (vals updateable-fields) [id])]
        (jdbc/execute-one! ds
                           (into [sql] params)
                           {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))))

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

(defn db-get-location [id]
  (println "Fetching location with ID:" id)
  (try
    (let [result (jdbc/execute-one! ds
                                    ["SELECT * FROM locations WHERE id = ?::uuid" id]
                                    {:builder-fn rs/as-unqualified-lower-maps})]
      (println "Location fetch result:" result)
      result)
    (catch Exception e
      (println "Error fetching location ID:" id "Error:" (.getMessage e))
      nil)))

(defn db-update-location [id loc]
  (let [now (current-timestamp)
        updateable-fields (select-keys loc [:label :name :type :description :parent_id :area])
        updateable-fields (assoc updateable-fields :updated_at now)
        required-fields [:name :type :area]
        existing-loc (db-get-location id)]
    (println "Attempting to update location ID:" id "with fields:" updateable-fields)
    (cond
      (nil? existing-loc)
      (do
        (println "Location not found for ID:" id)
        nil)
      (empty? (dissoc updateable-fields :updated_at))
      (do
        (println "No fields to update for location ID:" id)
        nil)
      (some #(and (contains? updateable-fields %) (or (nil? (get updateable-fields %)) (str/blank? (get updateable-fields %)))) required-fields)
      (do
        (println "Invalid or missing required field for location ID:" id "Fields:" updateable-fields)
        nil)
      (and (:parent_id updateable-fields)
           (not (and (valid-uuid? (:parent_id updateable-fields))
                     (db-get-location (:parent_id updateable-fields)))))
      (do
        (println "Invalid or non-existent parent_id for location ID:" id "parent_id:" (:parent_id updateable-fields))
        nil)
      :else
      (let [merged-fields (merge (select-keys existing-loc required-fields) updateable-fields)
            sql "UPDATE locations SET label = ?, name = ?, type = ?, description = ?, parent_id = ?::uuid, area = ?, updated_at = ? WHERE id = ?::uuid"
            params [(:label merged-fields) (:name merged-fields) (:type merged-fields)
                    (:description merged-fields) (:parent_id merged-fields) (:area merged-fields)
                    now id]]
        (println "Executing update query for location ID:" id "SQL:" sql "Params:" params)
        (try
          (let [result (jdbc/execute-one! ds
                                          (into [sql] params)
                                          {:return-keys true :builder-fn rs/as-unqualified-lower-maps})]
            (println "Update result for location ID:" id "Result:" result)
            result)
          (catch Exception e
            (println "Failed to update location ID:" id "Error:" (.getMessage e))
            (throw (ex-info "Database update failed" {:error (.getMessage e)}))))))))



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
