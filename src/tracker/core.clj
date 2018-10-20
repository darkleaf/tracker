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
  (< 0 (speed prev curr) speed-limit))

(defn remove-anomalies [data speed-limit]
  (reduce
   (fn [acc point]
     (let [last-point (peek acc)]
       (if (or (nil? last-point)
               (not-anomaly? speed-limit last-point point))
         (conj acc point)
         acc)))
   []
   data))


(defn compress [data n]
  (let [total-distance (reduce
                        (fn [{:keys [last-point] :as acc} point]
                          (if (nil? last-point)
                            (assoc acc :last-point point)
                            (-> acc
                                (assoc :last-point point)
                                (update :distance + (distance last-point point)))))
                        {:last-point nil, :distance 0}
                        data)
        step           (/ (:distance total-distance)  (dec n))
        result         (reduce
                        (fn [{:keys [points, last-point distance-acc, segment] :as acc} point]
                          (if (nil? last-point)
                            (-> acc
                                (update :points conj point)
                                (assoc :last-point point)
                                (assoc :segment 0))
                            (let [curr-segment (int (/ distance-acc step))]
                              (if (not= curr-segment segment)
                                (-> acc
                                    (update :points conj point)
                                    (assoc :last-point point)
                                    (update :distance-acc + (distance last-point point))
                                    (assoc :segment curr-segment))
                                (-> acc
                                    (assoc :last-point point)
                                    (update :distance-acc + (distance last-point point)))))))
                        {:points [], :last-point nil, :distance-acc 0, :segment nil}
                        data)]
    (:points result)))

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
