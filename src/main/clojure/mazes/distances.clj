(ns mazes.distances
  (require [mazes.cell :refer :all]))

(defn distances-from [cell]

  (defn next-pass [frontiers distances]
    (reduce (fn [cell state]
              (let [[new-frontiers distances] state]
                (doseq [linked (links cell)]
                  (if-not (contains? distances (to-id linked))
                    [(conj linked new-frontiers) (assoc distances (to-id linked) (inc ((to-id cell) distances)))]
                    state))))
            [(empty frontiers) distances] frontiers))

  (defn get-distances [frontiers distances]
    (if-not (empty? frontiers)
      (let [[f d] (next-pass frontiers distances)]
        (recur f d))
      distances))

  (get-distances [cell] {}))
