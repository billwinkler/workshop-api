(ns workshop-api.analyze-image-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ring.mock.request :as mock]
            [ring.util.response :refer [response status]]
            [ring.middleware.json :refer [wrap-json-body]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [cheshire.core :as json]
            [workshop-api.core :refer [app ds generate-id current-timestamp db-add-image db-add-image-analysis auth-backend valid-uuid?]]
            [buddy.sign.jwt :as jwt]
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
    (jdbc/execute! ds ["TRUNCATE TABLE item_images, location_images, items, images, image_analyses, locations, users RESTART IDENTITY"])
    (catch Exception e
      (println "Failed to truncate tables:" (.getMessage e)))))

(use-fixtures :each db-fixture)

;; Helper function for next.jdbc to query image_analyses with UUID casting
(defn find-by-keys-unqualified
  [ds table criteria]
  (let [{:keys [image_id]} criteria]
    (if (and (= table :image_analyses) image_id)
      (jdbc/execute! ds
                     ["SELECT * FROM image_analyses WHERE image_id = ?::uuid" image_id]
                     {:builder-fn rs/as-unqualified-lower-maps})
      (throw (ex-info "Unsupported table or criteria" {:table table :criteria criteria})))))

(deftest test-analyze-image
  (testing "Analyzing an image with valid JWT and config"
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
          config {:model_version "latest" :analysis_type "Image analysis"}
          request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                      (mock/json-body config)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))
          db-analysis (find-by-keys-unqualified ds :image_analyses {:image_id image-id})]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "analysis_started" (:status response-body)) "Expected analysis_started status")
      (is (= image-id (:image_id response-body)) "Expected correct image_id")
      (is (valid-uuid? (:analysis_id response-body)) "Expected valid UUID for analysis_id")
      (is (= 1 (count db-analysis)) "Expected one analysis record in database")
      (is (= "pending" (:status (first db-analysis))) "Expected pending status in database")))

  (testing "Analyzing an image without JWT"
    (let [image-id (generate-id)
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "test.jpg"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          config {:model_version "latest" :analysis_type "Image analysis"}
          request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                      (mock/json-body config))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))
          db-analysis (find-by-keys-unqualified ds :image_analyses {:image_id image-id})]
      (is (= 401 (:status response)) "Expected 401 status")
      (is (= "Unauthorized" (:message response-body)) "Expected unauthorized message")
      (is (empty? db-analysis) "Expected no analysis record in database")))

  (testing "Analyzing an image with invalid UUID"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image-id "invalid-uuid"
          config {:model_version "latest" :analysis_type "Image analysis"}
          request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                      (mock/json-body config)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid UUID format" (:error response-body)) "Expected error message")))

  (testing "Analyzing a non-existent image"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image-id (generate-id)
          config {:model_version "latest" :analysis_type "Image analysis"}
          request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                      (mock/json-body config)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))
          db-analysis (find-by-keys-unqualified ds :image_analyses {:image_id image-id})]
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Image not found" (:error response-body)) "Expected error message")
      (is (empty? db-analysis) "Expected no analysis record in database")))

  (testing "Analyzing an image with invalid config"
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
          config {:model_version 123 :analysis_type "Invalid"}
          request (-> (mock/request :post (str "/api/images/" image-id "/analyze"))
                      (mock/json-body config)
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))
          db-analysis (find-by-keys-unqualified ds :image_analyses {:image_id image-id})]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid analysis configuration" (:error response-body)) "Expected error message")
      (is (empty? db-analysis) "Expected no analysis record in database"))))

(deftest test-get-image-analysis
  (testing "Retrieving the most recent image analysis with valid JWT and fields"
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
          analysis-id (generate-id)
          analysis {:id analysis-id
                    :image_id image-id
                    :status "completed"
                    :model_version "latest"
                    :analysis_type "Image analysis"
                    :result (json/generate-string {:modelVersion "gemini-2.5-pro-exp-03-25"
                                                   :candidates [{:index 0
                                                                 :content {:role "model"
                                                                           :parts [{:text (json/generate-string {:description {:compartments [{:contents "Test compartment" :id "A1" :items ["Item1"]}]}
                                                                                                                 :reasoning "Test reasoning"
                                                                                                                 :summary "Test summary"})}]}
                                                                 :finishReason "STOP"}]})
                    :created_at (current-timestamp)
                    :updated_at (current-timestamp)}
          _ (db-add-image-analysis analysis)
          request (-> (mock/request :get (str "/api/images/" image-id "/analyze"))
                      (mock/query-string {:fields "status,summary,image_id,analysis_id"})
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= "completed" (:status response-body)) "Expected completed status")
      (is (= "Test summary" (:summary response-body)) "Expected correct summary")
      (is (= image-id (:image_id response-body)) "Expected correct image_id")
      (is (valid-uuid? (:analysis_id response-body)) "Expected valid UUID for analysis_id")
      (is (= analysis-id (:analysis_id response-body)) "Expected correct analysis_id")))

  (testing "Retrieving image analysis for non-existent image"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image-id (generate-id)
          request (-> (mock/request :get (str "/api/images/" image-id "/analyze"))
                      (mock/query-string {:fields "status"})
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Image analysis not found" (:error response-body)) "Expected error message")))

  (testing "Retrieving image analysis with invalid UUID"
    (let [user {:id (generate-id) :username "testuser"}
          payload {:user_id (:id user) :username (:username user)
                   :iat (quot (System/currentTimeMillis) 1000)
                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 60 60))}
          token (jwt/sign payload jwt-secret {:alg :hs256})
          image-id "invalid-uuid"
          request (-> (mock/request :get (str "/api/images/" image-id "/analyze"))
                      (mock/query-string {:fields "status"})
                      (assoc-in [:headers "Authorization"] (str "Bearer " token)))
          response (app request)
          response-body (try (json/parse-string (:body response) true)
                             (catch Exception e
                               (println "Failed to parse response:" (.getMessage e))
                               {}))]
      (is (= 400 (:status response)) "Expected 400 status")
      (is (= "Invalid UUID format" (:error response-body)) "Expected error message"))))
