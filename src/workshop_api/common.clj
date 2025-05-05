(ns workshop-api.common
  (:require [clojure.string :as str]))

(defn valid-uuid? [s]
  (println "Checking UUID, input:" s "type:" (type s))
  (if (string? s)
    (try
      (java.util.UUID/fromString s)
      (println "UUID valid:" s)
      true
      (catch IllegalArgumentException e
        (println "Invalid UUID:" s "Error:" (.getMessage e))
        false))
    (do
      (println "UUID input is not a string:" s)
      false)))
