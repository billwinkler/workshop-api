(ns workshop-api.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [cheshire.core :as json]
            [workshop-api.core :refer [app db-spec generate-id current-timestamp]]
            [clojure.string :as str]))

(def test-db-spec
  {:dbtype "postgresql" :dbname "workshop_inventory_test" :host "localhost" :user "billwinkler" :password (System/getenv "DB_PASSWORD")})

(def test-ds (jdbc/get-datasource test-db-spec))

;; Mock gemini/call-gemini-api to avoid external calls
(defn mock-gemini-api [image-data model-version analysis-type]
  {:mock_result (str "Analyzed " image-data " with " model-version " and " analysis-type)})

;; Fixture to set up and tear down the test database
(defn db-fixture [f]
  ;; Ensure tables exist (run this manually once to create tables)
  ;; Apply schema.sql to workshop_inventory_test before running tests
  (with-redefs [db-spec test-db-spec
                workshop-api.gemini-describe/call-gemini-api mock-gemini-api]
    (f))
  ;; Clean up: Truncate tables
  (jdbc/execute! test-ds ["TRUNCATE TABLE item_images, location_images, items, images, locations RESTART IDENTITY"]))

(use-fixtures :each db-fixture)

(deftest test-add-location
  (testing "Adding a valid location"
    (let [location {:label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"}
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location))
          response (app request)]
      (println "Location response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))
            db-location (sql/find-by-keys test-ds :locations {:name "Tool Shed"})]
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "Tool Shed" (:name response-body)) "Expected correct name in response")
        (is (or (nil? (:id response-body))
                (uuid? (java.util.UUID/fromString (:id response-body))))
            "Expected valid UUID in response")
        (is (= 1 (count db-location)) "Expected one location in database")
        (is (= "Tool Shed" (:name (first db-location))) "Expected correct name in database"))))
  (testing "Adding an invalid location (missing required fields)"
    (let [location {:label "L2"}  ;; Missing name, type, area
          request (-> (mock/request :post "/api/locations")
                      (mock/json-body location))
          response (app request)]
      (println "Invalid location response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid location format" (:error response-body)) "Expected error message")
        (is (empty? (sql/find-by-keys test-ds :locations {:label "L2"})) "Expected no location in database")))))

(deftest test-add-item
  (testing "Adding a valid item"
    ;; First, add a location for the item
    (let [location-id (generate-id)
          location {:id location-id
                    :label "L1"
                    :name "Tool Shed"
                    :type "Shed"
                    :area "Backyard"
                    :description "Storage for tools"
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (sql/insert! test-ds :locations location)
          item {:name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :category "Tool"
                :quantity 5}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Item response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))
            db-item (sql/find-by-keys test-ds :items {:name "Screwdriver"})]
        (is (= 200 (:status response)) "Expected 200 status")
        (is (= "Screwdriver" (:name response-body)) "Expected correct name in response")
        (is (or (nil? (:id response-body))
                (uuid? (java.util.UUID/fromString (:id response-body))))
            "Expected valid UUID in response")
        (is (= location-id (:location_id response-body)) "Expected correct location_id in response")
        (is (= 5 (:quantity response-body)) "Expected correct quantity in response")
        (is (= 1 (count db-item)) "Expected one item in database")
        (is (= "Screwdriver" (:name (first db-item))) "Expected correct name in database"))))
  (testing "Adding an invalid item (missing required fields)"
    (let [item {:category "Tool"}  ;; Missing name, description, location_id
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Invalid item response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid item format" (:error response-body)) "Expected error message")
        (is (empty? (sql/find-by-keys test-ds :items {:category "Tool"})) "Expected no item in database"))))
  (testing "Adding an item with non-existent location_id"
    (let [item {:name "Hammer"
                :description "Claw hammer"
                :location_id "00000000-0000-0000-0000-000000000000"}  ;; Non-existent UUID
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Invalid location_id response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Database error" (:error response-body)) "Expected error message")
        (is (empty? (sql/find-by-keys test-ds :items {:name "Hammer"})) "Expected no item in database")))))

(deftest test-add-image
  (testing "Adding a valid image"
    (let [image {:image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"}
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image))
          response (app request)]
      (println "Image response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))
            db-image (sql/find-by-keys test-ds :images {:filename "test.jpg"})]
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
    (let [image {:filename "invalid.jpg"}  ;; Missing image_data, mime_type
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image))
          response (app request)]
      (println "Invalid image response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid image format" (:error response-body)) "Expected error message")
        (is (empty? (sql/find-by-keys test-ds :images {:filename "invalid.jpg"})) "Expected no image in database")))))
