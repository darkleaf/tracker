(ns tracker.core
  (:require
   [tracker.reader :as treader]
   [tracker.writer :as twriter]
   [geo.spatial :as spatial]))

(defn speed [x y]
  (let [xp (-> x :point)
        yp (-> y :point)
        xt (-> x :at .toEpochMilli)
        yt (-> y :at .toEpochMilli)

        distance (spatial/distance xp yp)
        time     (/ (- yt xt) 1000)]
    (/ distance time)))

(defn distance [x y]
  (spatial/distance (:point x) (:point y)))

(defn not-anomaly? [speed-limit prev curr]
  (< (speed prev curr) speed-limit))

(defn remove-anomalies [points speed-limit]
  (reduce
   (fn [acc point]
     (let [last-point (peek acc)]
       (if (not-anomaly? speed-limit last-point point)
         (conj acc point)
         acc)))
   [(first points)]
   (drop 1 points)))

(defn compress [points n]
  (let [with-distance (reduce
                       (fn [acc point]
                         (let [last-point (-> acc peek :point)
                               dist       (-> acc peek :dist)]
                           (conj acc {:point point
                                      :dist (+ dist (distance last-point point))})))
                       [{:point (first points), :dist 0}]
                       (drop 1 points))
        total-distance (-> with-distance peek :dist)
        step           (/ total-distance (dec n))]
    (->> with-distance
         (partition-by (fn [{:keys [dist]}]
                         (int (/ dist step))))
         (map first)
         (map :point))))

(comment
  (let [data (treader/load-data "x.json")]
    (twriter/write "output.json" data)))

(comment
  (let [data (treader/load-data "y.json")]
    (twriter/write "output.json" data)))

(comment
  (let [data             (treader/load-data "x.json")
        speed-limit-km-h 120
        speed-limit      (/ (* speed-limit-km-h 1000) (* 60 60))
        filtered         (remove-anomalies data speed-limit)]

    (prn (count data)
         (count filtered))
    (twriter/write "output.json" filtered)))

(comment
  (let [data             (treader/load-data "x.json")
        speed-limit-km-h 120
        speed-limit      (/ (* speed-limit-km-h 1000) (* 60 60))
        filtered         (remove-anomalies data speed-limit)
        n                50
        compressed       (compress filtered n)]

    (prn (count data)
         (count filtered)
         (count compressed))
    (twriter/write "output.json" compressed)))
