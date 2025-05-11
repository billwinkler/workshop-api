(ns workshop-api.utils.git
  (:gen-class)
  (:require [taoensso.timbre :as log]
            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn load-config [config-file]
  (try
    (edn/read-string (slurp (io/resource config-file)))
    (catch Exception e
      (log/error "Error reading config file:" e)
      {})))

(def repos (load-config "repos.edn"))

(defn exists? [dir]
  (let [{:keys [exit]} (sh "ls" dir)]
    (zero? exit)))

(def repo (some #(when (exists? %) %) repos))
(log/debug "repo:" repo)
(log/debug "pwd:" (sh "pwd" :dir repo))
(log/debug "git:" (sh "git" "describe" "--tags" :dir repo))

(defn git-commit-hash []
  (try
    (let [{:keys [exit out]} (sh "git" "rev-parse" "--short" "HEAD" :dir repo)] ; Execute shell command to get the commit hash
      (if (zero? exit)
        (clojure.string/trim out)       ; Trim whitespace
        "Git hash not found"))          ; Handle errors
    (catch Exception _
      "Error getting Git hash")))

(defn git-describe-tags []
  (try
    (let [{:keys [exit out]} (sh "git" "describe" "--tags" :dir repo)] 
      (if (zero? exit)
        (clojure.string/trim out)  ; Trim whitespace
        "Git hash not found"))   ; Handle errors
    (catch Exception _
      "Error getting Git hash")))
