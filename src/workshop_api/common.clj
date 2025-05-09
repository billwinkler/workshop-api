(ns workshop-api.common
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))

(defn valid-uuid? [s]
  (log/debug "Checking UUID, input:" s "type:" (type s))
  (if (string? s)
    (try
      (java.util.UUID/fromString s)
      (log/debug "UUID valid:" s)
      true
      (catch IllegalArgumentException e
        (log/error "Invalid UUID:" s "Error:" (.getMessage e))
        false))
    (do
      (log/debug "UUID input is not a string:" s)
      false)))
