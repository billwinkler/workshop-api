(ns workshop-api.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [ring.util.response :refer [response status]]
            [ring.middleware.json :refer [wrap-json-body]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [cheshire.core :as json]
            [workshop-api.core :refer [app ds generate-id current-timestamp db-add-location
                                       db-get-image db-add-item db-add-image db-add-user prepare-user
                                       auth-backend]]
            [buddy.sign.jwt :as jwt]
            [buddy.hashers :as hashers]
            [buddy.auth.middleware :refer [wrap-authentication]]            
            [clojure.string :as str]))

(def jwt-secret (or (System/getenv "JWT_SECRET") "your-secure-secret-here"))
(def jwt-secret "justkidding")
;; Mock gemini/call-gemini-api to avoid external calls
(defn mock-gemini-api [image-data model-version analysis-type]
  (let [model-str (if (keyword? model-version)
                    (name model-version)
                    (str model-version))]
    {:mock_result (str "Analyzed " image-data " with " model-str " and " analysis-type)}))

;; Verify database schema
(defn verify-schema [ds]
  (let [result (jdbc/execute-one! ds
                                  ["SELECT EXISTS (
                                     SELECT FROM information_schema.tables
                                     WHERE table_name = 'locations') AS table_exists"]
                                  {:builder-fn rs/as-unqualified-lower-maps})]
    (if (:table_exists result)
      (println "Locations table exists")
      (throw (ex-info "Locations table does not exist" {})))))

;; Fixture to set up and tear down the test database
(defn db-fixture [f]
  (try
    (jdbc/execute-one! ds ["SELECT 1"])
    (catch Exception e
      (println "Test database setup failed:" (.getMessage e))
      (throw e)))
  (with-redefs [workshop-api.gemini-describe/call-gemini-api mock-gemini-api]
    (f))
  (try
    (jdbc/execute! ds ["TRUNCATE TABLE item_images, location_images, items, images, locations, users RESTART IDENTITY"])
    (catch Exception e
      (println "Failed to truncate tables:" (.getMessage e)))))

(use-fixtures :each db-fixture)

;; Helper function to check if a string is a valid UUID
(defn valid-uuid? [s]
  (try
    (java.util.UUID/fromString s)
    true
    (catch IllegalArgumentException _
      false)))

;; Helper function for next.jdbc to coerce unqualified keys
(defn find-by-keys-unqualified
  [ds table criteria]
  (sql/find-by-keys ds table criteria {:builder-fn rs/as-unqualified-lower-maps}))

;; Custom query for item_images to handle UUID casting
(defn find-item-images
  [ds criteria]
  (let [{:keys [item_id image_id]} criteria]
    (cond
      (and item_id image_id)
      (if (and (valid-uuid? item_id) (valid-uuid? image_id))
        (jdbc/execute! ds
                       ["SELECT * FROM item_images WHERE item_id = ?::uuid AND image_id = ?::uuid" item_id image_id]
                       {:builder-fn rs/as-unqualified-lower-maps})
        [])
      item_id
      (if (valid-uuid? item_id)
        (jdbc/execute! ds
                       ["SELECT * FROM item_images WHERE item_id = ?::uuid" item_id]
                       {:builder-fn rs/as-unqualified-lower-maps})
        [])
      image_id
      (if (valid-uuid? image_id)
        (jdbc/execute! ds
                       ["SELECT * FROM item_images WHERE image_id = ?::uuid" image_id]
                       {:builder-fn rs/as-unqualified-lower-maps})
        [])
      :else
      (throw (ex-info "At least one of item_id or image_id must be provided" {})))))

;; Add this helper function to query location_images, similar to find-item-images
(defn find-location-images
  [ds criteria]
  (let [{:keys [location_id image_id]} criteria]
    (cond
      (and location_id image_id)
      (if (and (valid-uuid? location_id) (valid-uuid? image_id))
        (jdbc/execute! ds
                       ["SELECT * FROM location_images WHERE location_id = ?::uuid AND image_id = ?::uuid" location_id image_id]
                       {:builder-fn rs/as-unqualified-lower-maps})
        [])
      location_id
      (if (valid-uuid? location_id)
        (jdbc/execute! ds
                       ["SELECT * FROM location_images WHERE location_id = ?::uuid" location_id]
                       {:builder-fn rs/as-unqualified-lower-maps})
        [])
      image_id
      (if (valid-uuid? image_id)
        (jdbc/execute! ds
                       ["SELECT * FROM location_images WHERE image_id = ?::uuid" image_id]
                       {:builder-fn rs/as-unqualified-lower-maps})
        [])
      :else
      (throw (ex-info "At least one of location_id or image_id must be provided" {})))))

(deftest test-add-location
  (testing "Adding a valid location"
    (let [location {:label "L1" :name "Tool Shed" :type "Shed" :area "Backyard" :description "Storage for tools"}
          request (-> (mock/request :post "/api/locations") (mock/json-body location))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                               (catch Exception e
                                 (println "Failed to parse response:" (.getMessage e))
                                 {}))
            db-location (find-by-keys-unqualified ds :locations {:name "Tool Shed"})]
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "Tool Shed" (:name response-body)) "Expected correct name in response")
        (is (or (nil? (:id response-body)) (uuid? (java.util.UUID/fromString (:id response-body)))) "Expected valid UUID in response")
        (is (= 1 (count db-location)) "Expected one location in database")
        (is (= "Tool Shed" (:name (first db-location))) "Expected correct name in database")))))

(deftest test-add-valid-item
  (testing "Adding a valid item"
    (let [location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          item {:name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :category "Tool"
                :quantity 5}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))
            db-item (find-by-keys-unqualified ds :items {:name "Screwdriver"})]
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "Screwdriver" (:name response-body)) "Expected correct name in response")
        (is (or (nil? (:id response-body))
                (uuid? (java.util.UUID/fromString (:id response-body))))
            "Expected valid UUID in response")
        (is (= location-id (:location_id response-body)) "Expected correct location_id in response")
        (is (= 5 (:quantity response-body)) "Expected correct quantity in response")
        (is (= 1 (count db-item)) "Expected one item in database")
        (is (= "Screwdriver" (:name (first db-item))) "Expected correct name in database")))))

(deftest test-add-invalid-item
  (testing "Adding an invalid item (missing required fields)"
    (let [item {:category "Tool"}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Invalid item response:" (:body response))
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid item format" (:error response-body)) "Expected error message")
        (is (empty? (find-by-keys-unqualified ds :items {:category "Tool"})) "Expected no item in database")))))

(deftest test-add-item-non-existent-location
  (testing "Adding an item with non-existent location_id"
    (let [item {:name "Hammer"
                :description "Claw hammer"
                :location_id "00000000-0000-0000-0000-000000000000"}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Invalid location_id response:" (:body response))
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Database error" (:error response-body)) "Expected error message")
        (is (empty? (find-by-keys-unqualified ds :items {:name "Hammer"})) "Expected no item in database")))))

(deftest test-add-image
  (testing "Adding a valid image"
    (let [image {:image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"}
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))
            db-image (find-by-keys-unqualified ds :images {:filename "test.jpg"})]
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "test.jpg" (:filename response-body)) "Expected correct filename in response")
        (is (or (nil? (:id response-body))
                (uuid? (java.util.UUID/fromString (:id response-body))))
            "Expected valid UUID in response")
        (is (= "pending" (:status response-body)) "Expected pending status in response")
        (is (= 1 (count db-image)) "Expected one image in database")
        (is (= "test.jpg" (:filename (first db-image))) "Expected correct filename in database")
        (is (= "pending" (:status (first db-image))) "Expected pending status in database"))))
  (testing "Adding an invalid image (missing required fields)"
    (let [image {:filename "invalid.jpg"}
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid image format" (:error response-body)) "Expected error message")
        (is (empty? (find-by-keys-unqualified ds :images {:filename "invalid.jpg"}))
            "Expected no image in database")))))

(deftest test-add-item-image
  (testing "Adding a valid item-image link"
    (let [location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          item-id (generate-id)
          item {:id item-id
                :name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :category "Tool"
                :quantity 5
                :created_at (current-timestamp)
                :updated_at (current-timestamp)}
          _ (db-add-item item)
          image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :status "pending"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          item-image {:item_id item-id :image_id image-id}
          request (-> (mock/request :post "/api/item-images")
                      (mock/json-body item-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))
            db-item-image (find-item-images ds {:item_id item-id :image_id image-id})]
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "success" (:status response-body)) "Expected success status in response")
        (is (= item-id (:item_id response-body)) "Expected correct item_id in response")
        (is (= image-id (:image_id response-body)) "Expected correct image_id in response")
        (is (= 1 (count db-item-image)) "Expected one item-image link in database")
        (is (= item-id (str (:item_id (first db-item-image)))) "Expected correct item_id in database")
        (is (= image-id (str (:image_id (first db-item-image)))) "Expected correct image_id in database"))))
  (testing "Adding item-image link with invalid UUIDs"
    (let [item-image {:item_id "invalid-uuid" :image_id "invalid-uuid"}
          request (-> (mock/request :post "/api/item-images")
                      (mock/json-body item-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid UUID format" (:error response-body)) "Expected error message")
        (is (empty? (find-item-images ds {:item_id "invalid-uuid"})) "Expected no item-image link in database"))))
  (testing "Adding item-image link with non-existent item"
    (let [image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :status "pending"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          item-image {:item_id "00000000-0000-0000-0000-000000000000" :image_id image-id}
          request (-> (mock/request :post "/api/item-images")
                      (mock/json-body item-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 404 (:status response)) "Expected 404 status")
        (is (= "Item or image not found" (:error response-body)) "Expected error message")
        (is (empty? (find-item-images ds {:item_id "00000000-0000-0000-0000-000000000000"})) "Expected no item-image link in database"))))
  (testing "Adding item-image link with non-existent image"
    (let [location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          item-id (generate-id)
          item {:id item-id
                :name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :category "Tool"
                :quantity 5
                :created_at (current-timestamp)
                :updated_at (current-timestamp)}
          _ (db-add-item item)
          item-image {:item_id item-id :image_id "00000000-0000-0000-0000-000000000000"}
          request (-> (mock/request :post "/api/item-images")
                      (mock/json-body item-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 404 (:status response)) "Expected 404 status")
        (is (= "Item or image not found" (:error response-body)) "Expected error message")
        (is (empty? (find-item-images ds {:item_id item-id})) "Expected no item-image link in database")))))

(deftest test-add-location-image
  (testing "Adding a valid location-image link"
    (let [location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :status "pending"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          location-image {:location_id location-id :image_id image-id}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))
            db-location-image (find-location-images ds {:location_id location-id :image_id image-id})]
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "success" (:status response-body)) "Expected success status in response")
        (is (= location-id (:location_id response-body)) "Expected correct location_id in response")
        (is (= image-id (:image_id response-body)) "Expected correct image_id in response")
        (is (= 1 (count db-location-image)) "Expected one location-image link in database")
        (is (= location-id (str (:location_id (first db-location-image)))) "Expected correct location_id in database")
        (is (= image-id (str (:image_id (first db-location-image)))) "Expected correct image_id in database"))))

  (testing "Adding location-image link with invalid UUIDs"
    (let [location-image {:location_id "invalid-uuid" :image_id "invalid-uuid"}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid UUID format" (:error response-body)) "Expected error message")
        (is (empty? (find-location-images ds {:location_id "invalid-uuid"})) "Expected no location-image link in database"))))

  (testing "Adding location-image link with non-existent location"
    (let [image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :status "pending"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          location-image {:location_id "00000000-0000-0000-0000-000000000000" :image_id image-id}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 404 (:status response)) "Expected 404 status")
        (is (= "Location or image not found" (:error response-body)) "Expected error message")
        (is (empty? (find-location-images ds {:location_id "00000000-0000-0000-0000-000000000000"})) "Expected no location-image link in database"))))

  (testing "Adding location-image link with non-existent image"
    (let [location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          location-image {:location_id location-id :image_id "00000000-0000-0000-0000-000000000000"}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 404 (:status response)) "Expected 404 status")
        (is (= "Location or image not found" (:error response-body)) "Expected error message")
        (is (empty? (find-location-images ds {:location_id location-id})) "Expected no location-image link in database")))))

(defn wait-for-image-status [ds image-id expected-status timeout-ms]
  (let [start-time (System/currentTimeMillis)]
    (loop []
      (let [db-image (jdbc/execute-one! ds
                                        ["SELECT * FROM images WHERE id = ?::uuid" image-id]
                                        {:builder-fn rs/as-unqualified-lower-maps})]
        (cond
          (= (:status db-image) expected-status) db-image
          (> (- (System/currentTimeMillis) start-time) timeout-ms) (throw (ex-info "Timeout waiting for image status" {:image-id image-id :expected-status expected-status}))
          :else (do (Thread/sleep 100) (recur)))))))

(deftest test-malformed-json
  (testing "Handling malformed JSON"
    (let [request (-> (mock/request :post "/api/items")
                      (assoc :body (java.io.ByteArrayInputStream. (.getBytes "{")))
                      (assoc-in [:headers "content-type"] "application/json")
                      (assoc-in [:headers "content-length"] "1"))
          response (app request)]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid JSON" (:body response)) "Expected error message"))))

(deftest test-json-body-middleware
  (testing "JSON body parsing"
    (let [handler (fn [request] (response {:received-body (:body request)}))
          wrapped-handler (wrap-json-body handler {:keywords? true :malformed-response {:status 400 :body "Invalid JSON"}})
          request (-> (mock/request :post "/test")
                      (mock/json-body {:model_version "latest" :analysis_type "Image analysis"}))
          body-str (slurp (:body request))
          request (assoc request :body (java.io.ByteArrayInputStream. (.getBytes body-str)))
          response (wrapped-handler request)
          response-body (:body response)]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (map? (:received-body response-body)) "Expected parsed JSON body to be a map")
      (is (= {:model_version "latest" :analysis_type "Image analysis"} (:received-body response-body))
          "Expected parsed JSON body"))))

(deftest test-analyze-image
  (testing "Analyzing an image"
    (db-fixture
     (fn []
       (jdbc/with-transaction [tx ds]
         (let [image-id (generate-id)
               insert-result (jdbc/execute-one! tx
                                                ["INSERT INTO images (id, image_data, mime_type, status, created_at, updated_at)
                                                 VALUES (?::uuid, ?, ?, ?, ?, ?)"
                                                 image-id "data" "image/jpeg" "pending" (current-timestamp) (current-timestamp)]
                                                {:builder-fn rs/as-unqualified-lower-maps :return-keys true})
               image-from-db (db-get-image image-id tx)
               request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                           (mock/json-body {:model_version "latest" :analysis_type "Image analysis"}))
               body-str (slurp (:body request))
               request (assoc request :body (java.io.ByteArrayInputStream. (.getBytes body-str)))
               response (app (assoc request :next.jdbc/connection tx))
               response-body (json/parse-string (:body response) true)]
           (is (= 200 (:status response)) "Expected 200 status")
           (is (= "analysis_started" (:status response-body)) "Expected analysis started status")
           (let [db-image (wait-for-image-status tx image-id "completed" 5000)
                 gemini-result (if (instance? org.postgresql.util.PGobject (:gemini_result db-image))
                                 (json/parse-string (.getValue (:gemini_result db-image)) true)
                                 (:gemini_result db-image))]
             (println "Processed gemini_result:" gemini-result)
             (is (= "completed" (:status db-image)) "Expected completed status")
             (is (map? gemini-result) "Expected gemini_result to be a map")
             (is (= {:mock_result "Analyzed data with latest and Image analysis"} gemini-result)
                 "Expected correct Gemini result"))))))))

(deftest test-jwt-sign-verify
  (let [jwt-secret "justkidding"
        payload {:user_id (generate-id) :username "testuser"}
        token (jwt/sign payload jwt-secret {:alg :hs256})
        verified (jwt/unsign token jwt-secret {:alg :hs256})]
    (is (= payload verified) "JWT signing and verification should match")))

(deftest test-jws-backend-minimal
  (let [user {:id (generate-id) :username "testuser"}
        payload {:user_id (:id user) :username (:username user)
                 :iat (quot (System/currentTimeMillis) 1000)
                 :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
        token (jwt/sign payload jwt-secret {:alg :hs256})
        request {:headers {"Authorization" (str "Bearer " token)}}
        handler (fn [req] req) ; Return request to inspect identity
        app (buddy.auth.middleware/wrap-authentication handler auth-backend)
        response (app request)]
    (println "jws-backend minimal test, token:" token)
    (println "jws-backend minimal test, request headers:" (:headers request))
    (println "jws-backend minimal test, identity:" (:identity response))
    (is (some? (:identity response)) "Expected auth data to be parsed")
    (is (= (:id user) (get-in response [:identity :user_id])) "Expected correct user_id")
    (is (= "testuser" (get-in response [:identity :username])) "Expected correct username")))

(deftest test-jws-backend
  (let [user {:id (generate-id) :username "testuser"}
        payload {:user_id (:id user) :username (:username user)
                 :iat (quot (System/currentTimeMillis) 1000)
                 :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
        token (jwt/sign payload jwt-secret {:alg :hs256})
        request {:headers {"Authorization" (str "Bearer " token)}}
        authenticated-request ((buddy.auth.middleware/wrap-authentication identity auth-backend) request)]
    (println "jws-backend test, token:" token)
    (println "jws-backend test, request headers:" (:headers request))
    (println "jws-backend test, identity:" (:identity authenticated-request))
    (is (some? (:identity authenticated-request)) "Expected auth data to be parsed")
    (is (= (:id user) (get-in authenticated-request [:identity :user_id])) "Expected correct user_id")
    (is (= "testuser" (get-in authenticated-request [:identity :username])) "Expected correct username")))

(deftest test-user-registration
  (testing "Registering a valid user"
    (let [user {:username "testuser" :password "securepassword123"}
          request (-> (mock/request :post "/api/register")
                      (mock/json-body user))
          response (app request)
          response-body (json/parse-string (:body response) true)
          db-user (find-by-keys-unqualified ds :users {:username "testuser"})]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "testuser" (:username (:user response-body))) "Expected correct username in response")
      (is (string? (:token response-body)) "Expected JWT token in response")
      (is (= 1 (count db-user)) "Expected one user in database")
      (is (= "testuser" (:username (first db-user))) "Expected correct username in database")
      (is (hashers/check "securepassword123" (:password_hash (first db-user))) "Expected correct password hash")))

  (testing "Registering with existing username"
    (let [user {:username "testuser" :password "securepassword123"}
          request (-> (mock/request :post "/api/register")
                      (mock/json-body user))
          response (app request)
          response-body (json/parse-string (:body response) true)]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Username already exists" (:error response-body)) "Expected error message")))

  (testing "Registering with invalid user data"
    (let [user {:username "ab" :password "short"}
          request (-> (mock/request :post "/api/register")
                      (mock/json-body user))
          response (app request)
          response-body (json/parse-string (:body response) true)]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid user format" (:error response-body)) "Expected error message"))))

(deftest test-user-login
  (testing "Logging in with valid credentials"
    (let [user {:username "testuser" :password "securepassword123"}
          _ (db-add-user (prepare-user user))
          request (-> (mock/request :post "/api/login")
                      (mock/json-body user))
          response (app request)
          response-body (json/parse-string (:body response) true)]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "testuser" (:username (:user response-body))) "Expected correct username in response")
      (is (string? (:token response-body)) "Expected JWT token in response")))

  (testing "Logging in with invalid credentials"
    (let [user {:username "testuser" :password "wrongpassword"}
          request (-> (mock/request :post "/api/login")
                      (mock/json-body user))
          response (app request)
          response-body (json/parse-string (:body response) true)]
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Invalid credentials" (:error response-body)) "Expected error message")))

  (testing "Logging in with missing credentials"
    (let [user {:username "testuser"}
          request (-> (mock/request :post "/api/login")
                      (mock/json-body user))
          response (app request)
          response-body (json/parse-string (:body response) true)]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Missing username or password" (:error response-body)) "Expected error message"))))

(deftest test-protected-route
  (testing "Accessing protected route with valid JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          _ (println "Generated token:" token)
          decoded (jwt/unsign token jwt-secret {:alg :hs256})
          _ (println "Decoded token:" decoded)
          location {:label "L1" :name "Tool Shed" :type "Shed" :area "Backyard" :description "Storage for tools"}
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try
                          (json/parse-string (:body response) true)
                          (catch Exception e
                            (println "Failed to parse response body:" (.getMessage e))
                            {}))]
      (println "Valid JWT response:" response)
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "Tool Shed" (:name response-body)) "Expected correct location name")
      (is (str/starts-with? (get-in response [:headers "Content-Type"]) "application/json") "Expected JSON content type")))

  (testing "Accessing protected route without JWT"
    (let [location {:label "L1" :name "Tool Shed" :type "Shed" :area "Backyard" :description "Storage for tools"}
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location))
          response (app request)
          response-body (try
                          (json/parse-string (:body response) true)
                          (catch Exception e
                            (println "Failed to parse response body:" (.getMessage e))
                            {}))]
      (println "No JWT response body:" (:body response))
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected error message")
      (is (str/starts-with? (get-in response [:headers "Content-Type"]) "application/json") "Expected JSON content type")))

  (testing "Accessing protected route with invalid JWT"
    (let [location {:label "L1" :name "Tool Shed" :type "Shed" :area "Backyard" :description "Storage for tools"}
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location)
                      (assoc-in [:headers "Authorization"] "Bearer invalid.token.here"))
          response (app request)
          response-body (try
                          (json/parse-string (:body response) true)
                          (catch Exception e
                            (println "Failed to parse response body:" (.getMessage e))
                            {}))]
      (println "Invalid JWT response body:" (:body response))
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected error message")
      (is (str/starts-with? (get-in response [:headers "Content-Type"]) "application/json") "Expected JSON content type"))))

