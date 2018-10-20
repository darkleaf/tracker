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

(defn window-2 [xf]
  (let [vprev (volatile! nil)]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result input]
       (let [prev @vprev]
         (vreset! vprev input)
         (xf result [prev input]))))))

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


(defn distance [x y]
  (spatial/distance (:point x) (:point y)))

(defn compress [data n]
  (let [total-distance (transduce (comp window
                                        (drop 1)
                                        (map (fn [[prev curr]]
                                               (distance prev curr))))
                                  + 0 filtered)]))

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




        ;; n                50
        ;; step             (/ total-distance n)
        ;; anchors          (for [i n] (* i step))
        ;; reduced          (into []
        ;;                        (comp
        ;;                         window

        ;;                         (map last))
        ;;                        filtered))))



#_(comment
    (let [x                (treader/load-data "x.json")
          speed-limit-km-h 120
          n                10
          speed-limit      (/ (* speed-limit-km-h 1000) (* 60 60))
          compressed       (into []
                                 (comp
                                  (remove-anomalies speed-limit)
                                  (compress 10))
                                 x)]
      (prn (count x)
           (count compressed))
      (twriter/write "output.json" compressed)))
