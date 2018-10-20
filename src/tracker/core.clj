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

(defn window-2 [xf]
  (let [vprev (volatile! nil)]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result input]
       (let [prev @vprev]
         (vreset! vprev input)
         (xf result {:item input, :previous prev}))))))

(defn with-reducer [f initial]
  (fn [xf]
    (let [vacc (volatile! initial)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [acc (vswap! vacc f input)]
           (xf result {:item input, :acc acc})))))))

(defn with-last  [xf]
  (let [inputs (java.util.ArrayList.)
        vlast  (volatile! nil)]
    (fn
      ([] (xf))
      ([result]
       (let [result' (reduce (fn [result input]
                               (xf result {:item input, :last @vlast}))
                             result
                             inputs)]
         (xf result')))
      ([result input]
       (.add inputs input)
       (vreset! vlast input)
       result))))

(defn not-anomaly? [speed-limit prev curr]
  (or
   (nil? prev)
   (< 0 (speed prev curr) speed-limit)))

(defn remove-anomalies [data speed-limit]
  (into []
        (comp window-2
              (filter (fn [{:keys [item previous]}]
                        (not-anomaly? speed-limit previous item)))
              (map :item))
        data))

(defn compress [data n]
  (let [distance-fn (fn [acc {:keys [item previous]}]
                      (cond
                        (nil? previous) acc
                        :else           (+ acc (distance previous item))))
        part-fn     (fn [item]
                      (let [distance       (-> item :item :acc)
                            total-distance (-> item :last :acc)
                            step           (/ total-distance (dec n))]
                        (int (/ distance step))))]
    (into []
          (comp
           window-2
           (with-reducer distance-fn 0)
           with-last
           (partition-by part-fn)
           (map first)
           (map :item)
           (map :item)
           (map :item))
          data)))

(comment
  (let [data (treader/load-data "x.json")]
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
