(ns workshop-api.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [ring.util.response :refer [response status]]
            [ring.middleware.json :refer [wrap-json-body]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [cheshire.core :as json]
            [workshop-api.core :refer [app ds generate-id current-timestamp db-add-location db-get-image]]
            [clojure.string :as str]))

;; Mock gemini/call-gemini-api to avoid external calls
(defn mock-gemini-api [image-data model-version analysis-type]
  (let [model-str (if (keyword? model-version)
                    (name model-version) ; Converts :latest to "latest"
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
;;    (println "Test database connection successful")
;;    (verify-schema ds)
    (catch Exception e
      (println "Test database setup failed:" (.getMessage e))
      (throw e)))
  (with-redefs [workshop-api.gemini-describe/call-gemini-api mock-gemini-api]
    (f))
  (try
    (jdbc/execute! ds ["TRUNCATE TABLE item_images, location_images, items, images, locations RESTART IDENTITY"])
;;    (println "Truncated tables successfully")
    (catch Exception e
      (println "Failed to truncate tables:" (.getMessage e)))))

(use-fixtures :each db-fixture)

;; a helper function for next.jdbc to coerce unqualified keys  
(defn find-by-keys-unqualified
  [ds table criteria]
  (sql/find-by-keys ds table criteria {:builder-fn rs/as-unqualified-lower-maps}))

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
          _ (do
              (println "Inserting location:" location)
              (db-add-location location))
          item {:name "Screwdriver"
                :description "Phillips head screwdriver"
                :location_id location-id
                :category "Tool"
                :quantity 5}
          request (-> (mock/request :post "/api/items")
                      (mock/json-body item))
          response (app request)]
      (println "Item response:" (:body response))
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
;;      (println "Image response:" (:body response))  ;; Debug output
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
    (let [image {:filename "invalid.jpg"}  ;; Missing image_data, mime_type
          request (-> (mock/request :post "/api/images")
                      (mock/json-body image))
          response (app request)]
;;      (println "Invalid image response:" (:body response))  ;; Debug output
      (let [response-body (try (json/parse-string (:body response) true)
                              (catch Exception e
                                (println "Failed to parse response:" (.getMessage e))
                                {}))]
        (is (= 400 (:status response)) "Expected 400 status")
        (is (= "Invalid image format" (:error response-body)) "Expected error message")
        (is (empty? (find-by-keys-unqualified ds :images {:filename "invalid.jpg"}))
            "Expected no image in database")))))

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

(deftest test-json-body-middleware
  (testing "JSON body parsing"
    (let [request (-> (mock/request :post "/test")
                      (mock/json-body {:model_version "latest" :analysis_type "Image analysis"}))
          body-str (slurp (:body request))
          request (assoc request :body (java.io.ByteArrayInputStream. (.getBytes body-str)))
          response (app request)
          response-body (json/parse-string (:body response) true)]
      (println "Raw JSON body:" body-str)
      (println "Response body:" response-body)
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
               _ (println "Insert result:" insert-result)
               image-from-db (db-get-image image-id tx)
               _ (println "Image from DB before request:" image-from-db)
               request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                           (mock/json-body {:model_version "latest" :analysis_type "Image analysis"}))
               body-str (slurp (:body request))
               request (assoc request :body (java.io.ByteArrayInputStream. (.getBytes body-str)))
               _ (println "Raw JSON body:" body-str)
               _ (println "Parsed body map:" (try
                                               (json/parse-string body-str true)
                                               (catch Exception e
                                                 (println "JSON parse error:" (.getMessage e))
                                                 nil)))
               response (app (assoc request :next.jdbc/connection tx))
               response-body (json/parse-string (:body response) true)]
           (println "Response status:" (:status response))
           (println "Response body:" response-body)
           (is (= 200 (:status response)) "Expected 200 status")
           (is (= "analysis_started" (:status response-body)) "Expected analysis started status")
           (let [db-image (wait-for-image-status tx image-id "completed" 5000)
                 gemini-result (if (instance? org.postgresql.util.PGobject (:gemini_result db-image))
                                 (json/parse-string (.getValue (:gemini_result db-image)) true)
                                 (:gemini_result db-image))]
             (println "Retrieved image from DB:" db-image)
             (println "Processed gemini_result:" gemini-result)
             (is (= "completed" (:status db-image)) "Expected completed status")
             (is (map? gemini-result) "Expected gemini_result to be a map")
             (is (= {:mock_result "Analyzed data with latest and Image analysis"} gemini-result)
                 "Expected correct Gemini result"))))))))



