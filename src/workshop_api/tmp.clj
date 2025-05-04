(ns workshop-api.routes
  (:require [compojure.core :refer :all]
            [ring.util.response :refer [response status]]
            [workshop-api.db :as db]
            [workshop-api.util :as util]
            [workshop-api.auth :as auth]
            [buddy.auth :refer [authenticated? throw-unauthorized]]))

; ... (all route handler functions remain unchanged) ...

(defroutes test-routes
  (POST "/test" request
    (response {:received-body (:body request)})))

(defroutes protected-routes
  (POST "/locations" request (add-location request))
  (PATCH "/locations/:id" [id :as request] (update-location request id))
  (DELETE "/locations/:id" [id] (delete-location id))
  (POST "/items" request (add-item request))
  (PATCH "/items/:id" [id :as request] (update-item request id))
  (DELETE "/items/:id" [id] (delete-item id))
  (POST "/images" request (add-image request))
  (POST "/images/:id/analyze" [id :as request] (analyze-image request id))
  (POST "/item-images" request (add-item-image request))
  (DELETE "/item-images/:item_id/:image_id" [item-id image-id] (delete-item-image item-id image-id))
  (POST "/location-images" request (add-location-image request))
  (DELETE "/location-images/:location_id/:image_id" [location-id image-id] (delete-location-image location-id image-id))
  (GET "/item-images" request (get-item-images request))
  (GET "/location-images" request (get-location-images request)))

(defroutes public-routes
  (GET "/locations/hierarchy" request (get-location-hierarchy request))
  (GET "/locations/:id" [id] (get-location-details id))
  (GET "/locations/:location_id/items" [location_id] (get-items-for-location location_id))
  (GET "/items/:id" [id] (get-item id))
  (GET "/search" request (search-inventory request))
  (GET "/items" request (get-all-items request))
  (GET "/locations" request (get-all-locations request))
  (GET "/location/:param" [param] (get-location-by-name-or-label param))
  (GET "/images/:id" [id] (get-image id))
  (GET "/images/:id/analyze" [id :as request] (get-image-analysis request id)))

(defroutes auth-routes
  (POST "/api/register" request (auth/register-user request))
  (POST "/api/login" request (auth/login-user request)))

(defroutes app-routes
  (context "/api" []
    (routes
      public-routes
      (util/wrap-auth protected-routes)))
  (ANY "*" request
    (println "Unmatched request:" (:uri request))
    (status (response {:error "Route not found" :uri (:uri request)}) 404)))
