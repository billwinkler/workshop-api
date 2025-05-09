(ns workshop-api.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [ring.util.response :refer [response status]]
            [ring.middleware.json :refer [wrap-json-body]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [cheshire.core :as json]
            [workshop-api.auth :refer [prepare-user auth-backend]]
            [workshop-api.db :refer [ds db-add-location db-get-image db-add-item db-add-image
                                     db-add-user db-add-item-image db-add-location-image
                                     db-get-image-analyses current-timestamp]]
            [workshop-api.core :refer [app]]
            [workshop-api.common :refer [valid-uuid?]]
            [workshop-api.util :refer [generate-id]]
            [buddy.sign.jwt :as jwt]
            [buddy.hashers :as hashers]
            [buddy.auth.middleware :refer [wrap-authentication]]
            [clojure.string :as str]))

(def jwt-secret (or (System/getenv "JWT_SECRET") "your-secure-secret-here"))

;; Mock gemini/call-gemini-api to avoid external calls
(defn mock-gemini-api [image-data model-version analysis-type]
  (let [start-time (System/currentTimeMillis)
        model-str (if (keyword? model-version)
                    (name model-version)
                    (str model-version))
        result {:mock_result (str "Analyzed " image-data " with " model-str " and " analysis-type)}
        end-time (System/currentTimeMillis)]
    (println "mock-gemini-api executed in" (- end-time start-time) "ms")
    result))

;; Verify database schema
(defn verify-schema [ds]
  (let [tables [:locations :images :items :item_images :location_images :users :image_analyses :location_types :location_areas :item_categories]
        results (map (fn [table]
                       (jdbc/execute-one! ds
                                          [(str "SELECT EXISTS (
                                                 SELECT FROM information_schema.tables
                                                 WHERE table_name = ?)") (name table)]
                                          {:builder-fn rs/as-unqualified-lower-maps}))
                     tables)]
    (doseq [[table result] (map vector tables results)]
      (if (:exists result)
        (println (str (name table) " table exists"))
        (throw (ex-info (str (name table) " table does not exist") {}))))))

;; Fixture to set up and tear down the test database
(defn db-fixture [f]
  (try
    (jdbc/execute-one! ds ["SELECT 1"])
    (catch Exception e
      (println "Test database setup failed:" (.getMessage e))
      (throw e)))
  (try
    (jdbc/execute! ds ["TRUNCATE TABLE item_images, location_images, items, images, image_analyses, locations, users, location_types, location_areas, item_categories RESTART IDENTITY"])
    (println "Database tables truncated successfully")
    ;; Insert seed data for location_types, location_areas, and item_categories
    (jdbc/execute! ds
                   ["INSERT INTO location_types (name, description, created_at, updated_at)
                     VALUES ('Warehouse', 'Large storage facility', NOW(), NOW()),
                            ('Office', 'Administrative workspace', NOW(), NOW()),
                            ('Shed', 'Tool shed', NOW(), NOW()),
                            ('Storage Room', 'Small storage area', NOW(), NOW())"])
    (jdbc/execute! ds
                   ["INSERT INTO location_areas (name, description, created_at, updated_at)
                     VALUES ('North Wing', 'North section of building', NOW(), NOW()),
                            ('Basement', 'Underground storage', NOW(), NOW()),
                            ('Backyard', 'Rear of building', NOW(), NOW()),
                            ('Floor 1', 'First floor area', NOW(), NOW())"])
    (jdbc/execute! ds
                   ["INSERT INTO item_categories (name, description, created_at, updated_at)
                     VALUES ('Hand Tool', 'Manual tools', NOW(), NOW()),
                            ('Power Tool', 'Electric tools', NOW(), NOW()),
                            ('Material', 'Construction materials', NOW(), NOW()),
                            ('Equipment', 'Heavy equipment', NOW(), NOW())"])
    (println "Seed data inserted for location_types, location_areas, and item_categories")
    (verify-schema ds)
    (with-redefs [workshop-api.gemini-describe/call-gemini-api mock-gemini-api]
      (f))
    (catch Exception e
      (println "Test database setup or truncation failed:" (.getMessage e))
      (throw e))))

(use-fixtures :each db-fixture)

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

;; Helper function to query location_images, similar to find-item-images
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

(deftest test-add-valid-item
  (testing "Adding a valid item with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          category (first (jdbc/execute! ds ["SELECT id FROM item_categories WHERE name = ?" "Hand Tool"]
                                        {:builder-fn rs/as-unqualified-lower-maps}))
          item {:name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :item_category_id (:id category)
                :quantity 5}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (println "Response status:" (:status response))
        (println "Response body:" response-body)
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "Screwdriver" (:name response-body)) "Expected correct name in response")
        (is (or (nil? (:id response-body))
                (uuid? (java.util.UUID/fromString (:id response-body))))
            "Expected valid UUID in response")
        (is (= location-id (:location_id response-body)) "Expected correct location_id in response")
        (is (= 5 (:quantity response-body)) "Expected correct quantity in response")
        (is (= 1 (count (find-by-keys-unqualified ds :items {:name "Screwdriver"}))) "Expected one item in database")
        (is (= "Screwdriver" (:name (first (find-by-keys-unqualified ds :items {:name "Screwdriver"})))) "Expected correct name in database"))))
  (testing "Adding a valid item without JWT"
    (let [location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          category (first (jdbc/execute! ds ["SELECT id FROM item_categories WHERE name = ?" "Hand Tool"]
                                        {:builder-fn rs/as-unqualified-lower-maps}))
          item {:name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :item_category_id (:id category)
                :quantity 5}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (println "Response status (no JWT):" (:status response))
        (println "Response body (no JWT):" response-body)
        (is (= 401 (:status response)) "Expected 401 status")
        (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")))))

(deftest test-add-invalid-item
  (testing "Adding an invalid item with JWT (missing required fields)"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          item {:category "Tool"}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (println "Invalid item response:" (:body response))
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid item format" (:error response-body)) "Expected error message")
        (is (empty? (find-by-keys-unqualified ds :items {:category "Tool"})) "Expected no item in database"))))
  (testing "Adding an invalid item without JWT"
    (let [item {:category "Tool"}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Invalid item response (no JWT):" (:body response))
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 401 (:status response)) "Expected 401 status")
        (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")))))
(deftest test-add-item-non-existent-location
  (testing "Adding an item with non-existent location_id with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          item {:name "Hammer"
                :description "Claw hammer"
                :location_id "00000000-0000-0000-0000-000000000000"}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (println "Invalid location_id response:" (:body response))
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Database error" (:error response-body)) "Expected error message")
        (is (empty? (find-by-keys-unqualified ds :items {:name "Hammer"})) "Expected no item in database"))))
  (testing "Adding an item with non-existent location_id without JWT"
    (let [item {:name "Hammer"
                :description "Claw hammer"
                :location_id "00000000-0000-0000-0000-000000000000"}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Invalid location_id response (no JWT):" (:body response))
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 401 (:status response)) "Expected 401 status")
        (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")))))

(deftest test-add-image
  (testing "Adding a valid image with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image {:image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"}
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
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
        (is (nil? (:status response-body)) "Expected no status in response")
        (is (= 1 (count db-image)) "Expected one image in database")
        (is (= "test.jpg" (:filename (first db-image))) "Expected correct filename in database")
        (is (nil? (:status (first db-image))) "Expected no status in database"))))
  (testing "Adding a valid image without JWT"
    (let [image {:image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"}
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 401 (:status response)) "Expected 401 status")
        (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))
  (testing "Adding an invalid image with JWT (missing required fields)"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image {:filename "invalid.jpg"}
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid image format" (:error response-body)) "Expected error message")
        (is (empty? (find-by-keys-unqualified ds :images {:filename "invalid.jpg"}))
            "Expected no image in database"))))
  (testing "Adding an invalid image without JWT"
    (let [image {:filename "invalid.jpg"}
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 401 (:status response)) "Expected 401 status")
        (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")))))

(deftest test-add-item-image
  (testing "Adding a valid item-image link with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
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
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          item-image {:item_id item-id :image_id image-id}
          request (-> (mock/request :post "/api/item-images")
                      (mock/json-body item-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
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
  (testing "Adding a valid item-image link without JWT"
    (let [location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
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
                                {}))]
        (is (= 401 (:status response)) "Expected 401 status")
        (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))
  (testing "Adding item-image link with invalid UUIDs with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          item-image {:item_id "invalid-uuid" :image_id "invalid-uuid"}
          request (-> (mock/request :post "/api/item-images")
                      (mock/json-body item-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid UUID format" (:error response-body)) "Expected error message")
        (is (empty? (find-item-images ds {:item_id "invalid-uuid"})) "Expected no item-image link in database"))))
  (testing "Adding item-image link with non-existent item with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          item-image {:item_id "00000000-0000-0000-0000-000000000000" :image_id image-id}
          request (-> (mock/request :post "/api/item-images")
                      (mock/json-body item-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 404 (:status response)) "Expected 404 status")
        (is (= "Item or image not found" (:error response-body)) "Expected error message")
        (is (empty? (find-item-images ds {:item_id "00000000-0000-0000-0000-000000000000"})) "Expected no item-image link in database"))))
  (testing "Adding item-image link with non-existent image with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
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
                      (mock/json-body item-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
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
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          location-image {:location_id location-id :image_id image-id}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
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
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-image {:location_id "invalid-uuid" :image_id "invalid-uuid"}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid UUID format" (:error response-body)) "Expected error message")
        (is (empty? (find-location-images ds {:location_id "invalid-uuid"})) "Expected no location-image link in database"))))
  (testing "Adding location-image link with non-existent location"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          location-image {:location_id "00000000-0000-0000-0000-000000000000" :image_id image-id}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 404 (:status response)) "Expected 404 status")
        (is (= "Location or image not found" (:error response-body)) "Expected error message")
        (is (empty? (find-location-images ds {:location_id "00000000-0000-0000-0000-000000000000"})) "Expected no location-image link in database"))))
  (testing "Adding location-image link with non-existent image"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          location-image {:location_id location-id :image_id "00000000-0000-0000-0000-000000000000"}
          request (-> (mock/request :post "/api/location-images")
                      (mock/json-body location-image)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)]
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 404 (:status response)) "Expected 404 status")
        (is (= "Location or image not found" (:error response-body)) "Expected error message")
        (is (empty? (find-location-images ds {:location_id location-id})) "Expected no location-image link in database")))))

(defn wait-for-image-status [ds analysis-id expected-status timeout-ms]
  (let [start-time (System/currentTimeMillis)]
    (loop []
      (let [db-analysis (db-get-image-analyses analysis-id)]
        (cond
          (= (:status db-analysis) expected-status) db-analysis
          (> (- (System/currentTimeMillis) start-time) timeout-ms) (throw (ex-info "Timeout waiting for image analysis status" {:analysis-id analysis-id :expected-status expected-status}))
          :else (do (Thread/sleep 100) (recur)))))))

(deftest test-get-item-images
  (testing "Getting item images with valid item_id and JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
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
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          _ (db-add-item-image item-id image-id)
          request (-> (mock/request :get (str "/api/item-images?item_id=" item-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= 1 (count response-body)) "Expected one image in response")
      (is (= image-id (:id (first response-body))) "Expected correct image_id in response")
      (is (= "test.jpg" (:filename (first response-body))) "Expected correct filename in response")))
  (testing "Getting item images without JWT"
    (let [item-id (generate-id)
          request (-> (mock/request :get (str "/api/item-images?item_id=" item-id)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")))
  (testing "Getting item images with invalid item_id"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          invalid-item-id "invalid-uuid"
          request (-> (mock/request :get (str "/api/item-images?item_id=" invalid-item-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid item_id format" (:error response-body)) "Expected error message")))
  (testing "Getting item images with non-existent item_id"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          non-existent-item-id "00000000-0000-0000-0000-000000000000"
          request (-> (mock/request :get (str "/api/item-images?item_id=" non-existent-item-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "No images found for item" (:error response-body)) "Expected error message"))))

(deftest test-get-location-images
  (testing "Getting location images with valid location_id and JWT"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          _ (db-add-location-image location-id image-id)
          request (-> (mock/request :get (str "/api/location-images?location_id=" location-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                            (catch Exception e
                              (println "Failed to parse response:" (.getMessage e))
                              {}))]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= 1 (count response-body)) "Expected one image in response")
      (is (= image-id (:id (first response-body))) "Expected correct image_id in response")
      (is (= "test.jpg" (:filename (first response-body))) "Expected correct filename in response")))
  (testing "Getting location images without JWT"
    (let [location-id (generate-id)
          request (-> (mock/request :get (str "/api/location-images?location_id=" location-id)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                            (catch Exception e
                              (println "Failed to parse response:" (.getMessage e))
                              {}))]
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")))
  (testing "Getting location images with invalid location_id"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          invalid-location-id "invalid-uuid"
          request (-> (mock/request :get (str "/api/location-images?location_id=" invalid-location-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                            (catch Exception e
                              (println "Failed to parse response:" (.getMessage e))
                              {}))]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid location_id format" (:error response-body)) "Expected error message")))
  (testing "Getting location images with non-existent location_id"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          non-existent-location-id "00000000-0000-0000-0000-000000000000"
          request (-> (mock/request :get (str "/api/location-images?location_id=" non-existent-location-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                            (catch Exception e
                              (println "Failed to parse response:" (.getMessage e))
                              {}))]
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "No images found for location" (:error response-body)) "Expected error message"))))

(deftest test-analyze-image
  (testing "Analyzing an image without JWT"
    (db-fixture
     (fn []
       (jdbc/with-transaction [tx ds]
         (let [image-id (generate-id)
               insert-result (jdbc/execute-one! tx
                                                ["INSERT INTO images (id, image_data, mime_type, created_at, updated_at)
                                                  VALUES (?::uuid, ?, ?, ?, ?)"
                                                 image-id "data" "image/jpeg" (current-timestamp) (current-timestamp)]
                                                {:builder-fn rs/as-unqualified-lower-maps :return-keys true})
               image-from-db (db-get-image image-id tx)
               request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                           (mock/json-body {:model_version "latest" :analysis_type "Image analysis"}))
               body-str (slurp (:body request))
               request (assoc request :body (java.io.ByteArrayInputStream. (.getBytes body-str)))
               response (app (assoc request :next.jdbc/connection tx))
               response-body (try (json/parse-string (:body response) true)
                                  (catch Exception e
                                    (println "Failed to parse response:" (.getMessage e))
                                    {}))]
           (is (= 401 (:status response)) "Expected 401 status")
           (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))))
  (testing "Analyzing a non-existent image with JWT"
    (db-fixture
     (fn []
       (jdbc/with-transaction [tx ds]
         (let [user {:id (generate-id) :username "testuser"}
               payload {:user_id (:id user) :username (:username user)
                        :iat (quot (System/currentTimeMillis) 1000)
                        :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
               token (jwt/sign payload jwt-secret {:alg :hs256})
               image-id "00000000-0000-0000-0000-000000000000"
               request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                           (mock/json-body {:model_version "latest" :analysis_type "Image analysis"})
                           (assoc-in [:headers "Authorization"] (str "Bearer " token)))
               body-str (slurp (:body request))
               request (assoc request :body (java.io.ByteArrayInputStream. (.getBytes body-str)))
               response (app (assoc request :next.jdbc/connection tx))
               response-body (try (json/parse-string (:body response) true)
                                  (catch Exception e
                                    (println "Failed to parse response:" (.getMessage e))
                                    {}))]
           (is (= 404 (:status response)) "Expected 404 status")
           (is (= "Image not found" (:error response-body)) "Expected error message"))))))
  (testing "Analyzing an image with invalid UUID"
    (db-fixture
     (fn []
       (jdbc/with-transaction [tx ds]
         (let [user {:id (generate-id) :username "testuser"}
               payload {:user_id (:id user) :username (:username user)
                        :iat (quot (System/currentTimeMillis) 1000)
                        :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
               token (jwt/sign payload jwt-secret {:alg :hs256})
               image-id "invalid-uuid"
               request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                           (mock/json-body {:model_version "latest" :analysis_type "Image analysis"})
                           (assoc-in [:headers "Authorization"] (str "Bearer " token)))
               body-str (slurp (:body request))
               request (assoc request :body (java.io.ByteArrayInputStream. (.getBytes body-str)))
               response (app (assoc request :next.jdbc/connection tx))
               response-body (try (json/parse-string (:body response) true)
                                  (catch Exception e
                                    (println "Failed to parse response:" (.getMessage e))
                                    {}))]
           (is (= 400 (:status response)) "Expected 400 status")
           (is (= "Invalid UUID format" (:error response-body)) "Expected error message")))))))

(deftest test-get-items-for-location
  (testing "Getting items for a valid location_id"
    (let [location-id (generate-id)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                             {:builder-fn rs/as-unqualified-lower-maps}))
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
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
          request (mock/request :get (str "/api/locations/" location-id "/items"))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= 1 (count response-body)) "Expected one item in response")
      (is (= item-id (:id (first response-body))) "Expected correct item_id in response")
      (is (= "Screwdriver" (:name (first response-body))) "Expected correct item name in response")
      (is (= "Tool Shed" (:location_path (first response-body))) "Expected correct location path in response")))
  (testing "Getting items for an invalid location_id"
    (let [invalid-location-id "invalid-uuid"
          request (mock/request :get (str "/api/locations/" invalid-location-id "/items"))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid location_id format" (:error response-body)) "Expected error message")))
  (testing "Getting items for a non-existent location_id"
    (let [non-existent-location-id "00000000-0000-0000-0000-000000000000"
          request (mock/request :get (str "/api/locations/" non-existent-location-id "/items"))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Location not found" (:error response-body)) "Expected error message"))))

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
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected error message")
      (is (str/starts-with? (get-in response [:headers "Content-Type"]) "application/json") "Expected JSON content type"))))
