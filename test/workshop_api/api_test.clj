(ns workshop-api.api-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [workshop-api.core :refer [ds generate-id current-timestamp db-add-image db-update-image]]
            [next.jdbc :as jdbc]))

;; Fixture to ensure the database is clean before and after each test
(defn db-fixture [f]
  (try
    (jdbc/execute-one! ds ["SELECT 1"])
    (catch Exception e
      (println "Test database setup failed:" (.getMessage e))
      (throw e)))
  (f)
  (try
    (jdbc/execute! ds ["TRUNCATE TABLE item_images, location_images, items, images, locations RESTART IDENTITY"])
    (catch Exception e
      (println "Failed to truncate tables:" (.getMessage e)))))

(use-fixtures :each db-fixture)

(defn get-image-by-id
  "Makes a GET request to the /api/images/:id endpoint and returns the parsed JSON response.
   Args:
     image-id: The UUID of the image to retrieve (as a string).
   Returns:
     A map containing :status (HTTP status code) and :body (parsed JSON response or error map)."
  [image-id]
  (let [url (str "http://localhost:3000/api/images/" image-id)]
    (try
      (let [response (http/get url
                               {:accept :json
                                :as :json
                                :throw-exceptions false
                                :headers {"Content-Type" "application/json"}})]
;;        (println "Raw response for" url ":" response)
        (let [body (if (string? (:body response))
                     (json/parse-string (:body response) true)
                     (:body response))]
          (println "Parsed body for" url ":" body)
          {:status (:status response)
           :body body}))
      (catch Exception e
        (println "Error fetching image from" url ":" (.getMessage e))
        {:status (or (:status (ex-data e)) 500)
         :body {:error (.getMessage e)
                :url url}}))))

(deftest test-get-image-by-id
  (testing "GET /api/images/:id with existing image"
    (let [image-id "a56a4b37-9721-4b08-b2b1-53a687de897e"
          image {:id image-id
                 :image_data "base64-encoded-data"
                 :mime_type "image/jpeg"
                 :filename "IMG_2528.jpeg"
                 :status "pending"
                 :created_at (current-timestamp)
                 :updated_at (current-timestamp)}
          _ (db-add-image image)
          _ (db-update-image image-id
                             {:status "completed"
                              :gemini_result (json/generate-string {:notes "Image analysis"})}
                             ds)
          response (get-image-by-id image-id)]
      (is (= 200 (:status response)) "Expected 200 status")
      (is (= image-id (:id (:body response))) "Response should contain the correct image ID")
      (is (= "completed" (:status (:body response))) "Image status should be completed")
      (is (= "IMG_2528.jpeg" (:filename (:body response))) "Filename should match the expected value")
      (is (map? (:gemini_result (:body response))) "gemini_result should be a parsed JSON map")
      (is (contains? (:gemini_result (:body response)) :notes) "gemini_result should contain notes")
      (is (= "Image analysis" (get-in (:body response) [:gemini_result :notes])) "gemini_result notes should match expected value")))
  (testing "GET /api/images/:id with non-existent image"
    (let [non-existent-id (generate-id)
          response (get-image-by-id non-existent-id)]
;;      (println "Response for non-existent ID" non-existent-id ":" response)
      (is (= 404 (:status response)) "Expected 404 status")
      (is (= "Image not found" (:error (:body response))) "Expected error message"))))
