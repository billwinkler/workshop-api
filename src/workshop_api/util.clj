(ns workshop-api.util
  (:require [clojure.string :as str]
            [workshop-api.db :as db]
            [workshop-api.common :refer [valid-uuid?]]
            [workshop-api.gemini-describe :as gemini]))

(defn keywordize-keys [m]
  (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) v]) m)))

(defn valid-location? [loc]
  (let [result (and (string? (:name loc))
                    (string? (:type loc))
                    (string? (:area loc))
                    (or (nil? (:label loc)) (string? (:label loc)))
                    (or (nil? (:description loc)) (string? (:description loc)))
                    (or (nil? (:parent_id loc)) (string? (:parent_id loc))))]
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
  (and (map? loc)
       (every? #(or (nil? (loc %)) (string? (loc %))) [:label :name :type :description :area])
       (every? #(or (nil? (loc %)) (not (str/blank? (loc %)))) [:name :label])
       (or (nil? (:parent_id loc)) (valid-uuid? (:parent_id loc)))))

(defn valid-image? [image]
  (let [result 
        (and (string? (:image_data image))
             (string? (:mime_type image))
             (or (nil? (:filename image)) (string? (:filename image))))]
    result))

(defn valid-analysis-config?-v0 [config]
  (let [model-version (if (string? (:model_version config))
                        (keyword (:model_version config))
                        (:model_version config))]
    (println "Processed model_version:" model-version)
    (and (or (nil? model-version) (keyword? model-version))
         (or (nil? (:analysis_type config)) (string? (:analysis_type config))))))

(defn valid-analysis-config? [config]
  (let [model-version (if (string? (:model_version config))
                        (keyword (:model_version config))
                        (:model_version config))]
    (println "Processed model_version:" model-version)
    (and (map? config)
         (or (keyword? model-version) (string? model-version))
         (string? (:analysis_type config))
         (not (clojure.string/blank? (:analysis_type config))))))

(defn generate-id [] (str (java.util.UUID/randomUUID)))

(defn prepare-location [loc]
  (let [now (db/current-timestamp)]
    (-> loc
        (assoc :id (generate-id))
        (assoc :created_at now)
        (assoc :updated_at now))))

(defn prepare-item [item]
  (let [now (db/current-timestamp)]
    (-> item
        (assoc :id (generate-id))
        (assoc :quantity (or (:quantity item) 1))
        (assoc :created_at now)
        (assoc :updated_at now))))

(defn prepare-image [image]
  (let [now (db/current-timestamp)]
    (-> image
        (assoc :id (generate-id))
        (assoc :created_at now)
        (assoc :updated_at now))))

(defn prepare-image-analysis [image_id config]
  (let [now (db/current-timestamp)]
    {:id (generate-id)
     :image_id image_id
     :status "pending"
     :model_version (or (:model_version config) "latest")
     :analysis_type (or (:analysis_type config) "Image analysis")
     :created_at now
     :updated_at now}))

(defn get-location-path [loc-id]
  (letfn [(build-path [id acc]
            (if-let [loc (db/db-get-location id)]
              (if (nil? (:parent_id loc))
                (conj acc (:name loc))
                (build-path (:parent_id loc) (conj acc (:name loc))))
              acc))]
    (str/join " > " (build-path loc-id []))))

(defn gemini-call [image-data model-version analysis-type]
  (gemini/call-gemini-api image-data model-version analysis-type))

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

