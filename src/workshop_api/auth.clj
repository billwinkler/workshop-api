(ns workshop-api.auth
  (:require [buddy.auth.backends.token :refer [jws-backend]]
            [buddy.hashers :as hashers]
            [buddy.sign.jwt :as jwt]
            [workshop-api.db :as db]))

(def jwt-secret (or (System/getenv "JWT_SECRET") "your-secure-secret-here"))

(def auth-backend
  (jws-backend
    {:secret jwt-secret
     :options {:alg :hs256}
     :on-error (fn [request error]
                 (println "JWT validation error for request:" (:headers request))
                 (println "Error message:" (.getMessage error)))
     :token-name "Bearer"}))

(defn valid-user? [user]
  (and (string? (:username user))
       (string? (:password user))
       (>= (count (:username user)) 3)
       (>= (count (:password user)) 8)))

(defn prepare-user [user]
  (let [now (db/current-timestamp)]
    (-> user
        (assoc :id (str (java.util.UUID/randomUUID)))
        (assoc :password_hash (hashers/derive (:password user) {:alg :bcrypt+sha512}))
        (dissoc :password)
        (assoc :created_at now)
        (assoc :updated_at now))))

(defn authenticate-user [username password]
  (if-let [user (db/db-get-user-by-username username)]
    (when (hashers/check password (:password_hash user))
      (dissoc user :password_hash))
    nil))

(defn register-user [request]
  (let [user (db/keywordize-keys (:body request))]
    (if (valid-user? user)
      (try
        (if (db/db-get-user-by-username (:username user))
          {:status 400 :body {:error "Username already exists"}}
          (let [new-user (prepare-user user)
                db-user (db/db-add-user new-user)
                token (jwt/sign {:user_id (:id db-user) :username (:username db-user)} jwt-secret {:alg :hs256})]
            {:status 200 :body {:user (dissoc db-user :password_hash) :token token}}))
        (catch Exception e
          {:status 500 :body {:error "Failed to register user" :message (.getMessage e)}}))
      {:status 400 :body {:error "Invalid user format" :data user}})))

(defn login-user [request]
  (let [{:keys [username password]} (db/keywordize-keys (:body request))]
    (if (and (string? username) (string? password))
      (if-let [user (authenticate-user username password)]
        (let [token (jwt/sign {:user_id (:id user) :username (:username user)} jwt-secret {:alg :hs256})]
          {:status 200 :body {:user user :token token}})
        {:status 401 :body {:error "Invalid credentials"}})
      {:status 400 :body {:error "Missing username or password"}})))
