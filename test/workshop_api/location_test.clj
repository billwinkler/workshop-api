(ns workshop-api.location-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [buddy.sign.jwt :as jwt]
            [workshop-api.auth :refer [auth-backend]]
            [workshop-api.db :refer [ds db-add-location db-get-location db-update-location db-check-location-items current-timestamp db-add-item]]
            [workshop-api.core :refer [app]]
            [workshop-api.common :refer [valid-uuid?]]
            [workshop-api.util :refer [generate-id]]
            [taoensso.timbre :as log]))

(def jwt-secret (or (System/getenv "JWT_SECRET") "your-secure-secret-here"))

;; Fixture to set up and tear down the test database

(defn db-fixture [f]
  (try
    (jdbc/execute-one! ds ["SELECT 1"])
    (println "Database connection verified")
    (jdbc/execute! ds
                   ["TRUNCATE TABLE locations, items, images, location_images, item_images, users, image_analyses, location_types, location_areas, item_categories RESTART IDENTITY CASCADE"])
    (println "Database tables truncated successfully")
    ;; Optional: Insert seed data for location_types, location_areas, and item_categories
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
                     VALUES ('Tools', 'Hand and power tools', NOW(), NOW()),
                            ('Electronics', 'Electronic devices', NOW(), NOW()),
                            ('Furniture', 'Office and storage furniture', NOW(), NOW())"])
    (println "Seed data inserted for location_types, location_areas, and item_categories")
    (f)
    (catch Exception e
      (println "Test database setup or truncation failed:" (.getMessage e))
      (throw e))))

(use-fixtures :each db-fixture)

;; Helper function to query locations by ID with UUID casting
(defn find-location-by-id
  [ds location-id]
  (if (valid-uuid? location-id)
    (jdbc/execute! ds
                   ["SELECT * FROM locations WHERE id = ?::uuid" location-id]
                   {:builder-fn rs/as-unqualified-lower-maps})
    []))

;; Helper function to create a JWT token
(defn create-jwt-token [user]
  (let [payload {:user_id (:id user)
                 :username (:username user)
                 :iat (quot (System/currentTimeMillis) 1000)
                 :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}]
    (jwt/sign payload jwt-secret {:alg :hs256})))

(deftest test-add-location
  (testing "Adding a valid location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          location {:label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (println "location:" location)
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))
          db-location (find-location-by-id ds (:id response-body))]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "Tool Shed" (:name response-body)) "Expected correct name in response")
      (is (or (nil? (:id response-body)) (valid-uuid? (:id response-body))) "Expected valid UUID in response")
      (is (= (:id location-type) (:location_type_id response-body)) "Expected correct location_type_id in response")
      (is (= (:id location-area) (:location_area_id response-body)) "Expected correct location_area_id in response")
      (is (= 1 (count db-location)) "Expected one location in database")
      (is (= "Tool Shed" (:name (first db-location))) "Expected correct name in database")
      (is (= (:id location-type) (:location_type_id (first db-location))) "Expected correct location_type_id in database")
      (is (= (:id location-area) (:location_area_id (first db-location))) "Expected correct location_area_id in database")))
  (testing "Adding a valid location without JWT"
    (let [location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          location {:label "L1"
                    :name "Tool Shed"
                    :location_type_id (:id location-type)
                    :location_area_id (:id location-area)
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))

(deftest test-update-location
  (testing "Updating a valid location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          location-id (generate-id)
          ;; Fetch valid location_type_id and location_area_id from seed data
          location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          ;; Create the location first
          initial-location {:id location-id
                            :label "L1"
                            :name "Tool Shed"
                            :location_type_id (:id location-type)
                            :location_area_id (:id location-area)
                            :description "Storage for tools"
                            :created_at (current-timestamp)
                            :updated_at (current-timestamp)}
          _ (db-add-location initial-location)
          _ (println "### successfully added location")
          update-data {:name "Updated Tool Shed"
                       :description "Updated storage for tools"
                       :label "L1"
                       :location_type_id (:id location-type)
                       :location_area_id (:id location-area)}
          _ (println "Updating location ID:" location-id "with data:" update-data)
          request (-> (mock/request :patch (str "/api/locations/" location-id))
                      (mock/json-body update-data)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))
          db-location (find-location-by-id ds location-id)]
      (println "Response body:" response-body)
      (println "Database location:" db-location)
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "Updated Tool Shed" (:name response-body)) "Expected updated name in response")
      (is (= "Updated storage for tools" (:description response-body)) "Expected updated description in response")
      (is (= 1 (count db-location)) "Expected one location in database")
      (is (= "Updated Tool Shed" (:name (first db-location))) "Expected updated name in database")))
  (testing "Updating a valid location with parent_id with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          parent-id (generate-id)
          parent-location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Warehouse"]
                                                    {:builder-fn rs/as-unqualified-lower-maps}))
          parent-location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Basement"]
                                                    {:builder-fn rs/as-unqualified-lower-maps}))
          parent-location {:id parent-id
                           :label "P1"
                           :name "Parent Location"
                           :location_type_id (:id parent-location-type)
                           :location_area_id (:id parent-location-area)
                           :description "Main storage facility"
                           :created_at (current-timestamp)
                           :updated_at (current-timestamp)}
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
          _ (db-add-location parent-location)
          _ (db-add-location location)
          update-data {:name "Updated Tool Shed"
                       :description "Updated storage for tools"
                       :label "L1"
                       :location_type_id (:id location-type)
                       :location_area_id (:id location-area)
                       :parent_id parent-id}
          _ (println "Updating location ID:" location-id "with data including parent_id:" update-data)
          request (-> (mock/request :patch (str "/api/locations/" location-id))
                      (mock/json-body update-data)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))
          db-location (find-location-by-id ds location-id)]
      (println "Response body:" response-body)
      (println "Database location:" db-location)
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "Updated Tool Shed" (:name response-body)) "Expected updated name in response")
      (is (= "Updated storage for tools" (:description response-body)) "Expected updated description in response")
      (is (= parent-id (:parent_id response-body)) "Expected updated parent_id in response")
      (is (= 1 (count db-location)) "Expected one location in database")
      (is (= "Updated Tool Shed" (:name (first db-location))) "Expected updated name in database")
      (is (= parent-id (str (:parent_id (first db-location)))) "Expected updated parent_id in database")))
  (testing "Updating a location with invalid parent_id with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
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
          update-data {:name "Updated Tool Shed"
                       :description "Updated storage for tools"
                       :label "L1"
                       :location_type_id (:id location-type)
                       :location_area_id (:id location-area)
                       :parent_id "00000000-0000-0000-0000-000000000000"}
          _ (println "Updating location ID:" location-id "with invalid parent_id:" update-data)
          request (-> (mock/request :patch (str "/api/locations/" location-id))
                      (mock/json-body update-data)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (println "Response for invalid parent_id:" response-body)
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid location format" (:error response-body)) "Expected error message")))
  (testing "Updating a location with associated items with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          parent-id (generate-id)
          parent-location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Warehouse"]
                                                    {:builder-fn rs/as-unqualified-lower-maps}))
          parent-location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Basement"]
                                                    {:builder-fn rs/as-unqualified-lower-maps}))
          parent-location {:id parent-id
                           :label "P1"
                           :name "Parent Location"
                           :location_type_id (:id parent-location-type)
                           :location_area_id (:id parent-location-area)
                           :description "Main storage facility"
                           :created_at (current-timestamp)
                           :updated_at (current-timestamp)}
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
          _ (db-add-location parent-location)
          _ (db-add-location location)
          item-category (first (jdbc/execute! ds ["SELECT id FROM item_categories WHERE name = ?" "Tools"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          item {:id (generate-id)
                :name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :item_category_id (:id item-category)
                :quantity 5
                :created_at (current-timestamp)
                :updated_at (current-timestamp)}
          _ (db-add-item item)
          update-data {:name "Updated Tool Shed"
                       :description "Updated storage for tools"
                       :label "L1"
                       :location_type_id (:id location-type)
                       :location_area_id (:id location-area)
                       :parent_id parent-id}
          _ (println "Updating location ID:" location-id "with associated item and parent_id:" update-data)
          request (-> (mock/request :patch (str "/api/locations/" location-id))
                      (mock/json-body update-data)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))
          db-location (find-location-by-id ds location-id)]
      (println "Response body:" response-body)
      (println "Database location:" db-location)
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "Updated Tool Shed" (:name response-body)) "Expected updated name in response")
      (is (= "Updated storage for tools" (:description response-body)) "Expected updated description in response")
      (is (= parent-id (:parent_id response-body)) "Expected updated parent_id in response")
      (is (= 1 (count db-location)) "Expected one location in database")
      (is (= "Updated Tool Shed" (:name (first db-location))) "Expected updated name in database")
      (is (= parent-id (str (:parent_id (first db-location)))) "Expected updated parent_id in database")))
  (testing "Updating a non-existent location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          non-existent-id "00000000-0000-0000-0000-000000000000"
          update-data {:name "Updated Tool Shed"}
          _ (println "Updating non-existent location ID:" non-existent-id "with data:" update-data)
          request (-> (mock/request :patch (str "/api/locations/" non-existent-id))
                      (mock/json-body update-data)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (println "Response for non-existent location:" response-body)
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Location not found" (:error response-body)) "Expected error message")))
  (testing "Updating a location with invalid data with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
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
          update-data {:name ""} ; Invalid: empty name
          _ (println "Updating location ID:" location-id "with invalid data:" update-data)
          request (-> (mock/request :patch (str "/api/locations/" location-id))
                      (mock/json-body update-data)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (println "Response for invalid data:" response-body)
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid location format" (:error response-body)) "Expected error message")))
  (testing "Updating a location without JWT"
    (let [location-id (generate-id)
          update-data {:name "Updated Tool Shed"}
          _ (println "Updating location ID:" location-id "without JWT, data:" update-data)
          request (-> (mock/request :patch (str "/api/locations/" location-id))
                      (mock/json-body update-data))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (println "Response for no JWT:" response-body)
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))

(deftest test-delete-location
  (testing "Deleting a valid location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
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
          request (-> (mock/request :delete (str "/api/locations/" location-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))
          db-location (find-location-by-id ds location-id)]
      (println "Response for delete valid location:" response-body)
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "success" (:status response-body)) "Expected success status in response")
      (is (empty? db-location) "Expected no location in database")))
  (testing "Deleting a non-existent location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          non-existent-id "00000000-0000-0000-0000-000000000000"
          request (-> (mock/request :delete (str "/api/locations/" non-existent-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (println "Response for delete non-existent location:" response-body)
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Location not found" (:error response-body)) "Expected error message")))
  (testing "Deleting a location with items with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
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
          item-category (first (jdbc/execute! ds ["SELECT id FROM item_categories WHERE name = ?" "Tools"]
                                              {:builder-fn rs/as-unqualified-lower-maps}))
          item {:id (generate-id)
                :name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :item_category_id (:id item-category)
                :quantity 5
                :created_at (current-timestamp)
                :updated_at (current-timestamp)}
          _ (db-add-item item)
          request (-> (mock/request :delete (str "/api/locations/" location-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))
          db-location (find-location-by-id ds location-id)]
      (println "Response for delete location with items:" response-body)
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Cannot delete location with items" (:error response-body)) "Expected error message")
      (is (= 1 (count db-location)) "Expected location to remain in database")))
  (testing "Deleting a location without JWT"
    (let [location-id (generate-id)
          request (-> (mock/request :delete (str "/api/locations/" location-id)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (println "Response for delete without JWT:" response-body)
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))

