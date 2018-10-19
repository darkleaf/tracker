(ns tracker.core
  (:require
   [tracker.reader :as treader]
   [tracker.writer :as twriter]
   [geo.spatial :as spatial]))

(defn distance [x y]
  (let [xp (-> x :point)
        yp (-> y :point)]
    (spatial/distance xp yp)))

(defn speed [x y]
  (let [xp (-> x :point)
        yp (-> y :point)
        xt (-> x :at .toEpochMilli)
        yt (-> y :at .toEpochMilli)

        distance (spatial/distance xp yp)
        time     (/ (- yt xt) 1000)]
    (/ distance time)))

;; speed-limit м/с
(defn remove-anomalies [speed-limit]
  (fn [xf]
    (let [prev (volatile! ::none)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if (and (not= ::none @prev)
                  (< speed-limit (speed @prev input)))
           result
           (do
             (vreset! prev input)
             (xf result input))))))))




(comment
  (let [x (treader/load-data "x.json")]
    (twriter/write "output.json" x)))


(comment
  (let [x                (treader/load-data "x.json")
        speed-limit-km-h 120
        speed-limit      (/ (* speed-limit-km-h 1000) (* 60 60))
        filtered         (into [] (remove-anomalies speed-limit) x)]
    (prn (count x)
         (count filtered))
    (twriter/write "output.json" filtered)))


