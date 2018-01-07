(ns main.clojure.mazes.grid-weaved)

(defn can-tunnel-fn [neighbors-fn grid]
  (fn [orientation cell]
    (let [neighbors (neighbors-fn (:row cell)
                                  (:column cell)
                                  grid)]
      (when-some [opposed-cell (orientation neighbors)]
        (comment "verify linked cells")))))


(defn neighbors-weaved-fn [neighbors-fn can-tunnel-fn]
  (fn [row col grid]
    (reduce-kv (fn [m k v]
                 (assoc m k (can-tunnel-fn k v)))
               {}
               (neighbors-fn row col grid))))

(defn is-tunnel-under-fn [fcell tcell neighbors-fn]
  (let [fcneigh (neighbors-fn (:row fcell)
                              (:column fcell))
        tcneigh (neighbors-fn (:row tcell)
                              (:column tcell))]
                                        ; Cool refactor possibility in here
    (if-some [tunnel-candidate (cond
                                 (-> fcneigh :north not-empty (= (:south tcneigh))) (:north fcneigh)
                                 (-> fcneigh :south not-empty (= (:north tcneigh))) (:sout fcneigh)
                                 (-> fcneigh :east not-empty (= (:west tcneigh)))   (:east fcneigh)
                                 (-> fcneigh :west not-empty (= (:east tcneigh)))   (:west fcneigh)
                                 :else                                             nil)]
      (comment "Define how we create tunnels here")
      nil)

    ))
