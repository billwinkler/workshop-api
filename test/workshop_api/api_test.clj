(ns workshop-api.api-test
  (:require [clojure.test :refer :all]
            [workshop-api.core :refer :all]
            [clj-http.client :as http]
            [cheshire.core :as json]))


(defn get-image-by-id
  "Makes a GET request to the /api/images/:id endpoint and returns the parsed JSON response.
   Args:
     image-id: The UUID of the image to retrieve (as a string).
   Returns:
     A Clojure map containing the parsed JSON response, or throws an exception on failure."
  [image-id]
  (let [url (str "http://localhost:3000/api/images/" image-id)]
    (try
      (let [response (http/get url
                               {:accept :json
                                :as :json})]
        (:body response))
      (catch Exception e
        (throw (ex-info (str "Failed to fetch image with ID " image-id)
                        {:url url
                         :status (:status (ex-data e))
                         :message (.getMessage e)}))))))

(deftest test-get-image-by-id
  (testing "GET /api/images/a56a4b37-9721-4b08-b2b1-53a687de897e"
    (let [image-id "a56a4b37-9721-4b08-b2b1-53a687de897e"
          response (get-image-by-id image-id)]
      (is (= image-id (:id response))
          "Response should contain the correct image ID")
      (is (= "completed" (:status response))
          "Image status should be completed")
      (is (= "IMG_2528.jpeg" (:filename response))
          "Filename should match the expected value")
      (is (map? (:gemini_result response))
          "gemini_result should be a parsed JSON map")
      (is (contains? (:gemini_result response) :notes)
          "gemini_result should contain notes")
      (is (= "Image analysis" (get-in (:gemini_result response) [:notes]))
          "gemini_result notes should match expected value"))))
