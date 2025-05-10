(ns workshop-api.utils.git
  (:gen-class)
  (:require [taoensso.timbre :as log]
            [clojure.java.shell :refer [sh]]))

(defn git-commit-hash []
  (try
    (let [{:keys [exit out]} (sh "git" "rev-parse" "--short" "HEAD")] ; Execute shell command to get the commit hash
      (if (zero? exit)
        (clojure.string/trim out)  ; Trim whitespace
        "Git hash not found"))   ; Handle errors
    (catch Exception _
      "Error getting Git hash")))

(defn git-describe-tags []
  (try
    (let [{:keys [exit out]} (sh "git" "describe" "--tags")] 
      (if (zero? exit)
        (clojure.string/trim out)  ; Trim whitespace
        "Git hash not found"))   ; Handle errors
    (catch Exception _
      "Error getting Git hash")))
