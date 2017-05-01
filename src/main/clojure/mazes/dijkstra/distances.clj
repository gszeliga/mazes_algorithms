(ns mazes.dijkstra.distances
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (cell-at)]))

(defn distances-from
  ([grid row col]
   (distances-from grid (cell-at grid row col)))

  ([grid cell]
   (defn next-pass [frontiers distances step]
     (reduce (fn [state linked]
               (let [[next-cells partial-distances] state]
                 (if-not (contains? partial-distances linked)
                   [(conj next-cells linked) (assoc partial-distances linked (inc step))]
                   state)))
             [(empty frontiers) distances]
             (mapcat identity (map #(links (apply cell-at grid %)) frontiers))))

   (defn get-distances [frontiers distances step]
     (if-not (empty? frontiers)
       (let [[f d] (next-pass frontiers distances step)]
         (recur f d (inc step)))
       distances))

   (let [cell-id (to-id cell)]
     (get-distances [cell-id] {cell-id 0} 0))))
