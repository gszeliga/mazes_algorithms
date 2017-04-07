(ns mazes.distances
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (cell-at)]))

(defn distances-from [cell grid]

  (defn next-pass [frontiers distances step]
    (reduce (fn [state linked]
              (let [[new-frontiers new-distances] state]
                (if-not (contains? new-distances linked)
                  [(conj new-frontiers linked) (assoc new-distances linked (inc step))]
                  state)))
            [(empty frontiers) distances]
            (mapcat identity (map #(links (apply cell-at grid %)) frontiers))))

  (defn get-distances [frontiers distances step]
    (if-not (empty? frontiers)
      (let [[f d] (next-pass frontiers distances step)]
        (recur f d (inc step)))
      distances))

  (get-distances [(to-id cell)] {(to-id cell) 0} 0))
