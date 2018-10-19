(ns tracker.core
  (:require
   [cheshire.core :as cheshire]
   [clojure.java.io :as io]))

(def x-json (-> "x.json" io/resource slurp (cheshire/decode true)))
(def y-json (-> "y.json" io/resource slurp (cheshire/decode true)))


(defn ->geo-json [data]
  (let [points (for [item data] (-> item :position reverse))]
    {:type       "Feature"
     :geometry   {:type        "LineString"
                  :coordinates points}
     :properties {}}))

(defn filter [data speed-limit])










(comment
  (as-> x-json <>
    (->geo-json <>)
    (cheshire/encode <> {:pretty true})
    (spit "output.json" <>)))
