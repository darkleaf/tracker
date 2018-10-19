(ns tracker.writer
  (:require
   [geo.jts :as jts]
   [geo.io :as gio]))

(defn write [name data]
  (->> data
       (map :point)
       (jts/linestring)
       (gio/to-geojson)
       (spit name)))
