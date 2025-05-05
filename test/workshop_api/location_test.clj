(ns workshop-api.location-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [buddy.sign.jwt :as jwt]
            [workshop-api.auth :refer [auth-backend]]
            [workshop-api.db :refer [ds db-add-location db-get-location db-update-location db-check-location-items current-timestamp]]
            [workshop-api.core :refer [app]]
            [workshop-api.util :refer [generate-id valid-uuid?]]))

(def jwt-secret (or (System/getenv "JWT_SECRET") "your-secure-secret-here"))

;; Fixture to set up and tear down the test database
(defn db-fixture [f]
  (try
    (jdbc/execute-one! ds ["SELECT 1"])
    (println "Database connection verified")
    (jdbc/execute! ds ["TRUNCATE TABLE locations, items, images, location_images, item_images, users, image_analyses RESTART IDENTITY"])
    (println "Database tables truncated successfully")
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
          location {:label "L1" :name "Tool Shed" :type "Shed" :area "Backyard" :description "Storage for tools"}
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
      (is (= 1 (count db-location)) "Expected one location in database")
      (is (= "Tool Shed" (:name (first db-location))) "Expected correct name in database")))
  (testing "Adding a valid location without JWT"
    (let [location {:label "L1" :name "Tool Shed" :type "Shed" :area "Backyard" :description "Storage for tools"}
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")))
  (testing "Adding an invalid location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          location {:label "L1"} ; Missing required fields
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid location format" (:error response-body)) "Expected error message")
      (is (empty? (find-location-by-id ds "00000000-0000-0000-0000-000000000000")) "Expected no location in database"))))

(deftest test-update-location
  (testing "Updating a valid location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          update-data {:name "Updated Tool Shed" :description "Updated storage for tools"}
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
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "Updated Tool Shed" (:name response-body)) "Expected updated name in response")
      (is (= "Updated storage for tools" (:description response-body)) "Expected updated description in response")
      (is (= 1 (count db-location)) "Expected one location in database")
      (is (= "Updated Tool Shed" (:name (first db-location))) "Expected updated name in database")))
  (testing "Updating a valid location with parent_id with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          parent-id (generate-id)
          parent-location {:id parent-id
                           :label "P1"
                           :name "Parent Location"
                           :type "Warehouse"
                           :area "Industrial Zone"
                           :description "Main storage facility"
                           :created_at (current-timestamp)
                           :updated_at (current-timestamp)}
          location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location parent-location)
          _ (db-add-location location)
          update-data {:name "Updated Tool Shed"
                       :description "Updated storage for tools"
                       :type "Shed"
                       :area "Backyard"
                       :label "L1"
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
          request (-> (mock/request :patch (str "/api/locations/" non-existent-id))
                      (mock/json-body update-data)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Location not found" (:error response-body)) "Expected error message")))
  (testing "Updating a location with invalid data with JWT"
    ;; This test expects util/valid-partial-location? to reject {:name ""} and return 400
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
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
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid location format" (:error response-body)) "Expected error message")))
  (testing "Updating a location without JWT"
    (let [location-id (generate-id)
          update-data {:name "Updated Tool Shed"}
          request (-> (mock/request :patch (str "/api/locations/" location-id))
                      (mock/json-body update-data))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))]
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))

(deftest test-delete-location
  (testing "Deleting a valid location with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
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
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Location not found" (:error response-body)) "Expected error message")))
  (testing "Deleting a location with items with JWT"
    (let [user {:id (generate-id) :username "testuser"}
          token (create-jwt-token user)
          location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-location location)
          item {:id (generate-id)
                :name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :category "Tool"
                :quantity 5
                :created_at (current-timestamp)
                :updated_at (current-timestamp)}
          _ (jdbc/execute-one! ds
                               ["INSERT INTO items (id, name, description, location_id, category, quantity, created_at, updated_at)
                                 VALUES (?::uuid, ?, ?, ?::uuid, ?, ?, ?, ?)"
                                (:id item) (:name item) (:description item) (:location_id item) (:category item) (:quantity item) (:created_at item) (:updated_at item)]
                               {:return-keys true})
          request (-> (mock/request :delete (str "/api/locations/" location-id))
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response, full response:" response)
                               (println "Error message:" (.getMessage e))
                               {:error (.getMessage e)}))
          db-location (find-location-by-id ds location-id)]
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
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message"))))
