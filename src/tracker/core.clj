(ns tracker.core
  (:require
   [tracker.reader :as treader]
   [tracker.writer :as twriter]
   [geo.spatial :as spatial]))

(defn speed-between [x y]
  (let [xp (-> x :point)
        yp (-> y :point)
        xt (-> x :at .toEpochMilli)
        yt (-> y :at .toEpochMilli)

        distance (spatial/distance xp yp)
        time     (/ (- yt xt) 1000)]
    (/ distance time)))

(defn distance-between [x y]
  (spatial/distance (:point x) (:point y)))

(defn not-anomaly? [speed-limit prev curr]
  (< (speed prev curr) speed-limit))

(defn remove-anomalies [points speed-limit]
  (reduce (fn [acc point]
            (let [last-point (peek acc)]
              (if (not-anomaly? speed-limit last-point point)
                (conj acc point)
                acc)))
          (->> points (take 1) vec)
          (drop 1 points)))

(defn- add-distance [points]
  (reduce (fn [acc point]
            (let [last-point (peek acc)
                  distance   (+ (::distance last-point)
                                (distance-between last-point point))
                  point      (assoc point ::distance distance)]
              (conj acc point)))
          (->> points
               (take 1)
               (map #(assoc % ::distance 0))
               (vec))
          (drop 1 points)))

(defn compress [points n]
  (let [with-distance  (add-distance points)
        total-distance (-> with-distance peek (get ::distance 0))
        step           (/ total-distance (dec n))]
    (->> with-distance
         (partition-by (fn [{::keys [distance]}]
                         (int (/ distance step))))
         (map first))))

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
