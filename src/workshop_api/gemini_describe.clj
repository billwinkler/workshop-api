(ns workshop-api.gemini-describe
  (:gen-class)
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [workshop-api.utils.apikey :as key] 
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;;(def model "gemini-2.0-flash")
;;(def model "gemini-2.5-pro-preview-03-25")
(def model "gemini-2.5-pro-exp-03-25")
(def model-base-url "https://generativelanguage.googleapis.com/v1beta/models/")

;; Load configurations from EDN files
(def model-configurations
  (edn/read-string (slurp (io/resource "model-configurations.edn"))))

(def instructions
  (edn/read-string (slurp (io/resource "instructions.edn"))))

(def prompts
  (edn/read-string (slurp (io/resource "prompts.edn"))))

(def schemas

  (edn/read-string (slurp (io/resource "schemas.edn"))))

(def configurations
  (edn/read-string (slurp (io/resource "configurations.edn"))))


(defn make-generation-config [config]
  (let [{:keys [settings schema]} (get configurations config)
        {:keys [max-output-tokens temperature top-p]} (get model-configurations settings)]
    {:temperature temperature
     :maxOutputTokens max-output-tokens
     :topP top-p
     :responseMimeType "application/json"
     :responseSchema  (get schemas schema)}))

(defn make-request-body [base64-image config]
  (let [{:keys [prompt instruction]} (get configurations config)]
    (json/generate-string {:contents [{:parts [{:text (get-in prompts [prompt :prompt])}
                                               {:inlineData {:mimeType "image/jpeg"
                                                             :data base64-image}}]}]
                           :systemInstruction {:parts [{:text (get-in instructions [instruction :system-instruction])}]}
                           :generationConfig (make-generation-config config)})))

(defn call-gemini-api [base64-image config notes]
  (let [api-key (or key/GEMINI_API_KEY
                    (throw (Exception. "GEMINI_API_KEY environment variable not set")))
        config (keyword config) ;; force the config to a keyword
        {:keys [model]} (get configurations config)
        _ (println "Model:" model)
        url (str model-base-url model ":generateContent?key=" api-key)
        body (make-request-body base64-image config)
        temp-file "/tmp/gemini_request.json"]
    (println "Preparing Gemini API call with model:" model "Config:" config "Notes:" notes)
;;    (println "Request URL:" url)
    (println "Request body size:" (count body) "bytes")
    (try
      (spit temp-file body)
      (println "Wrote request body to" temp-file)
      (let [max-retries 3
            retry-delay 10]
        (loop [attempt 1]
          (println "Attempting Gemini API call, attempt" attempt "of" max-retries)
          (let [response
                (try 
                  (http/post url
                             {:body body
                              :headers {"Content-Type" "application/json"}
                              :as :json})
                  (catch Exception e    ; <-- Correct use of catch
                    (println "API call failed:" (.getMessage e) "Status:" (:status (ex-data e)))
                    (println "Response headers:" (get-in (ex-data e) [:headers]))
                    (println "Response body:" (try
                                                  (slurp (get-in (ex-data e) [:body]))
                                                  (catch Exception e2
                                                    (println "Error reading response body:" (.getMessage e2))
                                                    "Unreadable")))
                    (if (and (= (:status (ex-data e)) 429) (< attempt max-retries))
                      (let [retry-after (or (some-> (get-in (ex-data e) [:headers :retry-after])
                                                    (Integer/parseInt))
                                            retry-delay)]
                        (println (str "Rate limit hit (attempt " attempt "/" max-retries "). Waiting " retry-after " seconds..."))
                        (Thread/sleep (* retry-after 1000))
                        ::retry)        ; Use a signal value for retry
                      (do
                        (println "Non-retryable error:" (.getMessage e))
                        (throw e)))))]  ; Re-throw original exception
            (if (= response ::retry)
              (recur (inc attempt))
              (do
                (println "API call successful, response received")
                (assoc (:body response) :notes notes))))))
      (catch Exception e
        (println "Error in Gemini API call:" (.getMessage e) "Stacktrace:" (seq (.getStackTrace e)))
        (throw e))
      (finally
;;        (io/delete-file temp-file :silent)
        ))))
  
  (defn finish-reason [api-response]
    (get-in api-response [:candidates 0 :finishReason]))

(defn usage-metadata [api-response]
  (:usageMetadata api-response))

(defn model-version [api-response]
  (:modelVersion api-response))

(defn parsed-content [api-response]
  (let [content (get-in api-response [:candidates 0 :content :parts 0 :text])]
    (json/parse-string content true)))

(comment
  (def api-response-v2
    (let [base64-image "xxxxxxx"]
      (call-gemini-api base64-image :latest "Test with lower temperature and top-p")))

  (get-in (parsed-content api-response-v9) [:description :compartments])
  
  (let [v1 api-response-v0
        v2 api-response-v2
        mapper (fn [resp] 
                 (map (fn [compartment] [(:id compartment)
                                         (apply str (take 50 (:contents compartment)))]) 
                      (get-in (parsed-content resp) [:description :compartments])))]

    (println "==== v1 ====")
    (println (get-in (parsed-content v1) [:reasoning]))
    (println "==== v2 ====")
    (println (get-in (parsed-content v2) [:reasoning]))
    (println "==== v1 ====")
    (doseq [line (mapper v1)] (println line))
    (println "==== v2 ====")
    (doseq [line (mapper v2)] (println line)))
  )

