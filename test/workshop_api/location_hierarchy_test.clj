(ns workshop-api.location-hierarchy-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [workshop-api.db :refer [ds db-add-location current-timestamp build-location-hierarchy]]
            [workshop-api.core :refer [app]]
            [workshop-api.util :refer [generate-id]]
            [taoensso.timbre :as log]))

;; Fixture to set up and tear down the test database
(defn db-fixture [f]
  (try
    ;; Truncate tables before each test to ensure a clean database
    (jdbc/execute! ds ["TRUNCATE TABLE locations, items, images, location_images, item_images, users, image_analyses, location_types, location_areas RESTART IDENTITY"])
    (println "Database tables truncated successfully")
    ;; Insert seed data for location_types and location_areas
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
    (log/info "Seed data inserted for location_types and location_areas")
    ;; Verify database connection
    (jdbc/execute-one! ds ["SELECT 1"])
    (log/debug "Database connection verified")
    (f)
    (catch Exception e
      (println "Test database setup or truncation failed:" (.getMessage e))
      (throw e))))

(use-fixtures :each db-fixture)

;; Helper function to coerce unqualified keys
(defn find-by-keys-unqualified
  [ds table criteria]
  (next.jdbc.sql/find-by-keys ds table criteria {:builder-fn rs/as-unqualified-lower-maps}))

(deftest test-get-location-hierarchy-nested
  (testing "Getting location hierarchy with nested locations"
    (let [parent-location-id (generate-id)
          parent-location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Warehouse"]
                                                     {:builder-fn rs/as-unqualified-lower-maps}))
          parent-location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "North Wing"]
                                                     {:builder-fn rs/as-unqualified-lower-maps}))
          parent-location {:id parent-location-id
                           :label "P1"
                           :name "Main Warehouse"
                           :location_type_id (:id parent-location-type)
                           :location_area_id (:id parent-location-area)
                           :description "Main storage facility"
                           :created_at (current-timestamp)
                           :updated_at (current-timestamp)}
          child-location-id (generate-id)
          child-location-type (first (jdbc/execute! ds ["SELECT id FROM location_types WHERE name = ?" "Shed"]
                                                    {:builder-fn rs/as-unqualified-lower-maps}))
          child-location-area (first (jdbc/execute! ds ["SELECT id FROM location_areas WHERE name = ?" "Backyard"]
                                                    {:builder-fn rs/as-unqualified-lower-maps}))
          child-location {:id child-location-id
                          :label "C1"
                          :name "Tool Section"
                          :location_type_id (:id child-location-type)
                          :location_area_id (:id child-location-area)
                          :description "Tool storage area"
                          :parent_id parent-location-id
                          :created_at (current-timestamp)
                          :updated_at (current-timestamp)}
          _ (db-add-location parent-location)
          _ (db-add-location child-location)
          request (mock/request :get "/api/locations/hierarchy")
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (log/error "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= 1 (count response-body)) "Expected one parent location in response")
      (let [parent (first response-body)]
        (is (= parent-location-id (:id parent)) "Expected correct parent location ID")
        (is (= "Main Warehouse" (:name parent)) "Expected correct parent location name")
        (is (= (:id parent-location-type) (:location_type_id parent)) "Expected correct parent location_type_id")
        (is (= (:id parent-location-area) (:location_area_id parent)) "Expected correct parent location_area_id")
        (is (= 1 (count (:children parent))) "Expected one child location")
        (let [child (first (:children parent))]
          (is (= child-location-id (:id child)) "Expected correct child location ID")
          (is (= "Tool Section" (:name child)) "Expected correct child location name")
          (is (= (:id child-location-type) (:location_type_id child)) "Expected correct child location_type_id")
          (is (= (:id child-location-area) (:location_area_id child)) "Expected correct child location_area_id"))))))
(deftest test-get-location-hierarchy-empty
  (testing "Getting location hierarchy with no locations"
    (let [request (mock/request :get "/api/locations/hierarchy")
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (empty? response-body) "Expected empty response for no locations"))))

(deftest test-get-location-hierarchy-error
  (testing "Getting location hierarchy with database error"
    (with-redefs [build-location-hierarchy (fn [] (throw (Exception. "Database connection failed")))]
      (let [request (mock/request :get "/api/locations/hierarchy")
            response (app request)
            response-body (try (json/parse-string (:body response) true)
                               (catch Exception e
                                 (println "Failed to parse response:" (.getMessage e))
                                 {}))]
        (is (= 500 (:status response)) "Expected 500 status")
        (is (= "Internal server error" (:error response-body)) "Expected error message")
        (is (= "Database connection failed" (:message response-body)) "Expected specific error message")))))
