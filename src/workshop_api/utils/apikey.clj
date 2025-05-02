(ns workshop-api.utils.apikey
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

   (def api-keys
     (edn/read-string (slurp (io/resource "apikey.edn"))))

   (def CHATGPT-KEY (:chatgpt api-keys))
   (def XAI_API_KEY (:xai api-keys))
   (def GEMINI_API_KEY (:gemini api-keys))


