(ns workshop-api.routes
  (:require [compojure.core :refer :all]
            [ring.util.response :refer [response status]]
            [clojure.core.async :refer [thread]]
            [workshop-api.db :as db]
            [workshop-api.util :as util]
            [workshop-api.common :refer [valid-uuid?]]
            [workshop-api.auth :as auth]
            [buddy.auth :refer [authenticated? throw-unauthorized]]
            [cheshire.core :as json]))

(defn add-location [request]
  (let [loc (db/keywordize-keys (:body request))]
    (if (util/valid-location? loc)
      (let [new-loc (util/prepare-location loc)]
        (db/db-add-location new-loc)
        (response new-loc))
      (status (response {:error "Invalid location format" :data loc}) 400))))

(defn update-location [request id]
  (let [loc (db/keywordize-keys (:body request))]
    (println "Validating partial location:" loc "Result:" (util/valid-partial-location? loc))
    (when (instance? java.io.InputStream (:body request))
      (try
        (.close (:body request))
        (catch Exception e
          (println "Error closing request body stream:" (.getMessage e)))))
    (if (util/valid-partial-location? loc)
      (if-let [existing-loc (db/db-get-location id)]
        (if-let [updated-loc (db/db-update-location id loc)]
          (response updated-loc)
          (status (response {:error "Invalid location format" :data loc}) 400))
        (status (response {:error "Location not found"}) 404))
      (status (response {:error "Invalid location format" :data loc}) 400))))

(defn delete-location [id]
  (if-let [loc (db/db-get-location id)]
    (let [{item-count :count} (db/db-check-location-items id)]
      (if (zero? item-count)
        (do
          (db/db-delete-location id)
          (response {:status "success"}))
        (status (response {:error "Cannot delete location with items"}) 400)))
    (status (response {:error "Location not found"}) 404)))

(defn add-item [request]
  (let [item (db/keywordize-keys (:body request))]
    (if (map? (:body request))
      (if (util/valid-item? item)
        (try
          (let [new-item (util/prepare-item item)]
            (if (db/db-get-location (:location_id new-item))
              (do
                (db/db-add-item new-item)
                (response new-item))
              (status (response {:error "Database error" :message "Invalid location_id"}) 400)))
          (catch Exception e
            (status (response {:error "Database error" :message (.getMessage e)}) 400)))
        (status (response {:error "Invalid item format" :data item}) 400))
      (do
        (println "add-item error: body is not a map, received:" (:body request))
        (status (response {:error "Invalid request body" :data (:body request)}) 400)))))

(defn get-all-items [_request]
  (let [items (db/db-get-all-items)]
    (response items)))

(defn update-item [request id]
  (let [item (db/keywordize-keys (:body request))]
    (try
      (if (util/valid-partial-item? item)
        (if-let [existing-item (db/db-get-item id)]
          (if-let [updated-item (db/db-update-item id item)]
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
  (if-let [item (db/db-get-item id)]
    (do
      (db/db-delete-item id)
      (response {:status "success"}))
    (status (response {:error "Item not found"}) 404)))

(defn get-location-details [id]
  (if-let [loc (db/db-get-location id)]
    (let [items (db/db-get-items-by-location id)
          images (db/db-get-location-images id)]
      (response {:location loc :items items :images images}))
    (status (response {:error "Location not found"}) 404)))

(defn get-all-locations [_request]
  (let [locations (db/db-get-all-locations)]
    (response locations)))

(defn get-item [id]
  (try
    (if-let [item (db/db-get-item id)]
      (let [item-with-path (assoc item :location_path (util/get-location-path (:location_id item)))
            images (db/db-get-item-images id)]
        (response (assoc item-with-path :images images)))
      (status (response {:error "Item not found"}) 404))
    (catch Exception e
      (println "Error in get-item for ID:" id "Error:" (.getMessage e) "Stacktrace:" (.getStackTrace e))
      (status (response {:error "Internal server error" :message (.getMessage e)}) 500))))

(defn add-item-image [request]
  (let [{:keys [item_id image_id]} (db/keywordize-keys (:body request))]
    (if (and (valid-uuid? item_id) (valid-uuid? image_id))
      (try
        (if (and (db/db-get-item item_id) (db/db-get-image image_id))
          (do
            (db/db-add-item-image item_id image_id)
            (response {:status "success" :item_id item_id :image_id image_id}))
          (status (response {:error "Item or image not found"}) 404))
        (catch Exception e
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid UUID format" :data {:item_id item_id :image_id image_id}}) 400))))

(defn add-location-image [request]
  (let [{:keys [location_id image_id]} (db/keywordize-keys (:body request))]
    (if (and (valid-uuid? location_id) (valid-uuid? image_id))
      (try
        (if (and (db/db-get-location location_id) (db/db-get-image image_id))
          (do
            (db/db-add-location-image location_id image_id)
            (response {:status "success" :location_id location_id :image_id image_id}))
          (status (response {:error "Location or image not found"}) 404))
        (catch Exception e
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid UUID format" :data {:location_id location_id :image_id image_id}}) 400))))

(defn delete-item-image [item-id image-id]
  (if (and (valid-uuid? item-id) (valid-uuid? image-id))
    (if (db/db-get-item item-id)
      (if (db/db-get-image image-id)
        (do
          (db/db-delete-item-image item-id image-id)
          (response {:status "success"}))
        (status (response {:error "Image not found"}) 404))
      (status (response {:error "Item not found"}) 404))
    (status (response {:error "Invalid UUID format" :data {:item_id item-id :image_id image-id}}) 400)))

(defn delete-location-image [location-id image-id]
  (if (and (valid-uuid? location-id) (valid-uuid? image-id))
    (if (db/db-get-location location-id)
      (if (db/db-get-image image-id)
        (do
          (db/db-delete-location-image location-id image-id)
          (response {:status "success"}))
        (status (response {:error "Image not found"}) 404))
      (status (response {:error "Location not found"}) 404))
    (status (response {:error "Invalid UUID format" :data {:location_id location-id :image_id image-id}}) 400)))

(defn get-item-images [request]
  (let [item-id (get-in request [:query-params "item_id"])
        token (get-in request [:headers "authorization"])]
    (println "get-item-images: item_id =" item-id ", token =" token)
    (if (valid-uuid? item-id)
      (let [images (db/db-get-item-images item-id)]
        (if (seq images)
          (response images)
          (status (response {:error "No images found for item" :item_id item-id}) 404)))
      (status (response {:error "Invalid item_id format" :item_id item-id}) 400))))

(defn get-location-images [request]
  (let [location-id (get-in request [:query-params "location_id"])
        token (get-in request [:headers "authorization"])]
    (println "get-location-images: location_id =" location-id ", token =" token)
    (if (valid-uuid? location-id)
      (let [images (db/db-get-location-images location-id)]
        (if (seq images)
          (response images)
          (status (response {:error "No images found for location" :location_id location-id}) 404)))
      (status (response {:error "Invalid location_id format" :location_id location-id}) 400))))

(defn add-image [request]
  (let [image (db/keywordize-keys (:body request))]
    (if (util/valid-image? image)
      (try
        (let [new-image (util/prepare-image image)]
          (db/db-add-image new-image)
          (response new-image))
        (catch Exception e
          (println "Error adding image:" (.getMessage e))
          (status (response {:error "Database error" :message (.getMessage e)}) 400)))
      (status (response {:error "Invalid image format" :data image}) 400))))

(defn analyze-image-v0 [request id]
  (println "Analyzing image ID:" id)
  (let [conn (or (:next.jdbc/connection request) db/ds)]
    (cond
      (not (valid-uuid? id))
      (do
        (println "Invalid UUID format:" id)
        (status (response {:error "Invalid UUID format" :id id}) 400))

      (not (db/db-get-image id conn))
      (do
        (println "Image not found for ID:" id)
        (status (response {:error "Image not found"}) 404))

      :else
      (let [config (db/keywordize-keys (:body request))]
        (println "Config:" config)
        (if (not (util/valid-analysis-config? config))
          (do
            (println "Invalid analysis config:" config)
            (status (response {:error "Invalid analysis configuration" :data config}) 400))
          (try
            (let [image (db/db-get-image id conn)
                  analysis (util/prepare-image-analysis id config)]
              (println "Starting analysis for image ID:" id)
              (db/db-add-image-analysis analysis conn)
              (thread
                (try
                  (println "Processing analysis ID:" (:id analysis))
                  (db/db-update-image-analysis (:id analysis) {:status "processing"} conn)
                  (let [result (util/gemini-call (:image_data image) (:model_version analysis) (:analysis_type analysis))]
                    (println "Gemini API result for analysis ID:" (:id analysis))
                    (db/db-update-image-analysis (:id analysis)
                                              {:status "completed"
                                               :result (json/generate-string result)}
                                              conn)
                    (println "Analysis completed for ID:" (:id analysis)))
                  (catch Exception e
                    (println "Error in analysis ID:" (:id analysis) "Error:" (.getMessage e))
                    (db/db-update-image-analysis (:id analysis)
                                              {:status "failed"
                                               :error_message (.getMessage e)}
                                              conn)
                    (println "Analysis failed for ID:" (:id analysis)))))
              (response {:status "analysis_started" :image_id id :analysis_id (:id analysis)}))
            (catch Exception e
              (println "Error starting analysis for ID:" id "Error:" (.getMessage e))
              (status (response {:error "Database error" :message (.getMessage e)}) 500))))))))

(defn analyze-image [request id]
  (println "Analyzing image ID:" id)
  (let [conn (or (:next.jdbc/connection request) db/ds)]
    (cond
      (not (valid-uuid? id))
      (do
        (println "Invalid UUID format:" id)
        (status (response {:error "Invalid UUID format" :id id}) 400))

      (not (db/db-get-image id conn))
      (do
        (println "Image not found for ID:" id)
        (status (response {:error "Image not found"}) 404))

      :else
      (let [config (db/keywordize-keys (:body request))]
        (println "Config:" config)
        (if (not (util/valid-analysis-config? config))
          (do
            (println "Invalid analysis config:" config)
            (status (response {:error "Invalid analysis configuration" :data config}) 400))
          (try
            (let [image (db/db-get-image id conn)
                  analysis (util/prepare-image-analysis id config)]
              (println "Starting analysis for image ID:" id)
              (db/db-add-image-analysis analysis conn)
              (thread
                (try
                  (println "Processing analysis ID:" (:id analysis))
                  (db/db-update-image-analysis (:id analysis) {:status "processing"} conn)
                  (let [result (util/gemini-call (:image_data image) (:model_version analysis) (:analysis_type analysis))]
                    (println "Gemini API result for analysis ID:" (:id analysis))
                    (db/db-update-image-analysis (:id analysis)
                                                {:status "completed"
                                                 :result (cheshire.core/generate-string result)}
                                                conn)
                    (println "Analysis completed for ID:" (:id analysis)))
                  (catch Exception e
                    (println "Error in analysis ID:" (:id analysis) "Error:" (.getMessage e))
                    (db/db-update-image-analysis (:id analysis)
                                                {:status "failed"
                                                 :error_message (.getMessage e)}
                                                conn)
                    (println "Analysis failed for ID:" (:id analysis)))))
              (response {:status "analysis_started" :image_id id :analysis_id (:id analysis)}))
            (catch Exception e
              (println "Error starting analysis for ID:" id "Error:" (.getMessage e))
              (status (response {:error "Database error" :message (.getMessage e)}) 500))))))))

(defn get-image-analysis [request image-id]
  (try
    (println "### Processing get-image-analysis for image-id:" image-id)
    (println "### Raw query-params:" (:query-params request))
    (println "### Is UUID valid?" (valid-uuid? image-id))
    (if (valid-uuid? image-id)
      (let [fields-str (get-in request [:query-params "fields"])
            allowed-fields #{"status" "summary" "error_message" "analysis_type"
                             "finish_reason" "usage_metadata" "model_version" "reasoning" "compartments"
                             "image_id" "analysis_id"}
            requested-fields (if (clojure.string/blank? fields-str)
                               #{"status" "image_id" "analysis_id" "model_version" "analysis_type"}
                               (set (map clojure.string/trim (clojure.string/split fields-str #","))))
            invalid-fields (clojure.set/difference requested-fields allowed-fields)]
        (println "### Fields string:" fields-str)
        (println "### Requested fields:" requested-fields "Invalid fields:" invalid-fields)
        (if (seq invalid-fields)
          (do
            (println "### Returning 400 due to invalid fields")
            (status (response {:error "Invalid fields requested" :invalid_fields invalid-fields}) 400))
          (if-let [analysis (first (db/db-get-image-analyses image-id db/ds))]
            (let [status (:status analysis)
                  base-response (-> (select-keys analysis [:image_id :model_version :analysis_type :status :error_message])
                                    (assoc :analysis_id (:id analysis)))
                  analysis-fields #{"finish_reason" "usage_metadata" "model_version" "reasoning" "summary" "compartments"}
                  result-fields (clojure.set/intersection requested-fields analysis-fields)
                  field-mappings {"model_version" :modelVersion
                                  "usage_metadata" :usageMetadata
                                  "finish_reason" :finishReason
                                  "summary" :summary
                                  "reasoning" :reasoning
                                  "compartments" :compartments}
                  result-data (when (and (:result analysis) (seq result-fields))
                                (let [result (:result analysis)
                                      text-json (try
                                                  (let [text (get-in result [:candidates 0 :content :parts 0 :text])]
                                                    (cheshire.core/parse-string text true))
                                                  (catch Exception e
                                                    (println "Error parsing text JSON for image ID:" image-id "Error:" (.getMessage e))
                                                    {}))]
                                  (into {} (map (fn [field]
                                                  [(keyword field)
                                                   (case field
                                                     "compartments" (get-in text-json [:description :compartments])
                                                     "finish_reason" (get-in result [:candidates 0 :finishReason])
                                                     "summary" (:summary text-json)
                                                     "reasoning" (:reasoning text-json)
                                                     (get result (get field-mappings field (keyword field))))])
                                                result-fields))))
                  remaining-fields #{"status" "image_id" "analysis_id" "model_version" "analysis_type" "error_message"}
                  response-data (merge
                                 (select-keys base-response
                                              (map keyword (clojure.set/intersection requested-fields remaining-fields)))
                                 result-data)]
              (println "### Returning analysis response for status:" status)
              (cond
                (= status "failed") (response (if (contains? requested-fields "error_message")
                                                response-data
                                                (dissoc response-data :error_message)))
                (= status "completed") (response response-data)
                :else (response response-data)))
            (do
              (println "### No analysis found for image-id:" image-id)
              (status (response {:error "Image analysis not found"}) 404)))))
      (do
        (println "### Returning 400 due to invalid UUID:" image-id)
        (status (response {:error "Invalid UUID format" :id image-id}) 400)))
    (catch Exception e
      (println "### Error in get-image-analysis for image ID:" image-id "Error:" (.getMessage e))
      (status (response {:error "Internal server error" :message (.getMessage e)}) 500))))

(defn get-image [id]
  (if-let [image (db/db-get-image id)]
    (let [analyses (db/db-get-image-analyses id)]
      (response (assoc image :analyses analyses)))
    (status (response {:error "Image not found"}) 404)))

(defn get-images [request]
  (try
    (println "Processing get-images")
    (let [fields-str (get-in request [:query-params "fields"])
          allowed-fields #{"id" "image_data" "mime_type" "filename" "status" "created_at" "updated_at"}
          requested-fields (if (clojure.string/blank? fields-str)
                             #{"id" "filename" "mime_type" "status"}
                             (set (map clojure.string/trim (clojure.string/split fields-str #","))))
          invalid-fields (clojure.set/difference requested-fields allowed-fields)]
      (println "Fields string:" fields-str)
      (println "Requested fields:" requested-fields "Invalid fields:" invalid-fields)
      (if (seq invalid-fields)
        (do
          (println "Returning 400 due to invalid fields")
          (status (response {:error "Invalid fields requested" :invalid_fields invalid-fields}) 400))
        (let [images (db/db-get-images requested-fields)]
          (if (seq images)
            (response images)
            (status (response {:error "No images found"}) 404)))))
    (catch Exception e
      (println "Error in get-images:" (.getMessage e))
      (status (response {:error "Internal server error" :message (.getMessage e)}) 500))))

(defn get-items-for-location [location-id]
  (if (valid-uuid? location-id)
    (if (db/db-get-location location-id)
      (let [items (db/db-get-items-by-location location-id)]
        (response (map #(assoc % :location_path (util/get-location-path (:location_id %))) items)))
      (status (response {:error "Location not found" :location_id location-id}) 404))
    (status (response {:error "Invalid location_id format" :location_id location-id}) 400)))

(defn get-location-hierarchy [_request]
  (try
    (let [hierarchy (db/build-location-hierarchy)]
      (response hierarchy))
    (catch Exception e
      (println "Error in get-location-hierarchy:" (.getMessage e))
      (status (response {:error "Internal server error" :message (.getMessage e)}) 500))))

(defn get-location-by-name-or-label [param]
  (let [normalized-param (clojure.string/replace param "+" " ")
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
         (if-let [loc (db/db-get-location id)]
           (let [loc-with-path (assoc loc :location_path (util/get-location-path (:id loc)))]
             (println "Found location by ID:" loc-with-path)
             (response loc-with-path))
           (do
             (println "Location not found for ID:" id)
             (status (response {:error "Location not found" :id id}) 404))))
       (when-let [loc (db/db-get-location-by-name normalized-param)]
         (let [loc-with-path (assoc loc :location_path (util/get-location-path (:id loc)))]
           (println "Found location by name:" loc-with-path)
           (response loc-with-path)))
       (when-let [loc (db/db-get-location-by-label normalized-param)]
         (let [loc-with-path (assoc loc :location_path (util/get-location-path (:id loc)))]
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
        items (if (clojure.string/blank? query)
                []
                (db/db-search-items query))]
    (response items)))

(defn wrap-auth [handler]
  (fn [request]
    (if (authenticated? request)
      (handler request)
      (do (println "in wrap-auth, throwing unauthorized:")
          (throw-unauthorized {:message "Unauthorized"})))))

(defroutes test-routes
  (POST "/test" request
    (response {:received-body (:body request)})))

(defroutes protected-routes
  (POST "/locations" request (add-location request))
  (PATCH "/locations/:id" [id :as request]
    (println "Matched route PATCH /locations/:id" id)
    (let [response (update-location request id)]
      (println "Handler response for id" id ":" response)
      response))
  (DELETE "/locations/:id" [id] (delete-location id))
  (POST "/items" request (add-item request))
  (PATCH "/items/:id" [id :as request] (update-item request id))
  (DELETE "/items/:id" [id]
    (println "### Matched route DELETE /images/:id with id:" id)
    (let [response (delete-item id)]
      (println "### Handler response for id" id ":" response)
      response))
  (POST "/images" request (add-image request))
  (POST "/images/:id/analyze" [id :as request] (analyze-image request id))
  (POST "/item-images" request (add-item-image request))
  (DELETE "/item-images/:item_id/:image_id" [item_id image_id]
    (delete-item-image item_id image_id))
  (POST "/location-images" request (add-location-image request))
  (DELETE "/location-images/:location_id/:image_id" [location_id image_id]
    (delete-location-image location_id image_id))
  (GET "/item-images" request (get-item-images request))
  (GET "/location-images" request (get-location-images request)))

(defroutes public-routes
  (GET "/locations/hierarchy" request (get-location-hierarchy request))
  (GET "/locations/:id" [id] (get-location-details id))
  (GET "/locations/:location_id/items" [location_id] (get-items-for-location location_id))
  (GET "/items/:id" [id] (get-item id))
  (GET "/search" request (search-inventory request))
  (GET "/items" request (get-all-items request))
  (GET "/locations" request (get-all-locations request))
  (GET "/location/:param" [param] (get-location-by-name-or-label param))
  (GET "/images/:id" [id] (get-image id))
  (GET "/images/:id/analyze" [id :as request]
    (println "Matched route /images/:id/analyze with id:" id)
    (let [response (get-image-analysis request id)]
      (println "Handler response for id" id ":" response)
      response))
  (GET "/images" request
    (println "Matched route /images")
    (let [response (get-images request)]
      (println "Handler response:" response)
      response)))

(defroutes auth-routes
  (POST "/api/register" request (auth/register-user request))
  (POST "/api/login" request (auth/login-user request)))

(defroutes app-routes
  (context "/api" []
    (routes
      public-routes
      (wrap-auth protected-routes)))
  (ANY "*" request
    (println "Unmatched request - URI:" (:uri request) "Method:" (:request-method request))
    (status (response {:error "Route not found" :uri (:uri request)}) 404)))
