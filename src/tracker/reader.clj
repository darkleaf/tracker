(ns tracker.reader
  (:require
   [cheshire.core :as cheshire]
   [clojure.java.io :as io]
   [geo.jts :as jts])
  (:import
   [java.time Instant]))

(defn- normalize-point [p]
  (let [at         (:at p)
        [lat long] (:position p)]
    {:at    (Instant/parse at)
     :point (jts/coordinate long lat)}))

(defn load-data [name]
  (as-> name <>
    (io/resource <>)
    (slurp <>)
    (cheshire/decode <> true)
    (map normalize-point <>)))

(comment
  (load-data "x.json"))
