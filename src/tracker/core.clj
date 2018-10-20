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
         (xf result [prev input]))))))

(defn with-stat [f initial]
  (fn [xf]
    (let [vstat (volatile! initial)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [stat (vswap! vstat f input)]
           (xf result [input stat])))))))

(defn not-anomaly? [speed-limit prev curr]
  (or
   (nil? prev)
   (< 0 (speed prev curr) speed-limit)))

(defn remove-anomalies [data speed-limit]
  (into []
        (comp window-2
              (filter (fn [[prev curr]]
                        (not-anomaly? speed-limit prev curr)))
              (map last))
        data))

(defn compress [data n]
  (let [stat-fn        (fn [stat [prev curr]]
                         (cond
                           (nil? prev) stat
                           :else       (+ stat (distance prev curr))))
        total-distance (transduce (comp window-2
                                        (with-stat stat-fn 0)
                                        (map last))
                                  (fn
                                    ([acc] acc)
                                    ([_acc item] item))
                                  0 data)
        step           (/ total-distance (dec n))]
    (into []
          (comp window-2
                (with-stat stat-fn 0)
                (partition-by (fn [[_ distance]]
                                (int (/ distance step))))
                (map first)
                (map first)
                (map last))
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
