(ns mazes.algorithms.sidewinder
  (use [mazes.grid :only (rows-from cells-from neighbors)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-emiter)])
  (require [mazes.cell :refer :all]))

(defn visit
  ([grid] (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-emiter f))

   (doseq [row (rows-from grid)]

     (defn traverse-cells [tmp remaining]

       (if-let [cell (first remaining)]

         (let [cell-neighbors (neighbors cell grid)
               eastern-neighbor (:east cell-neighbors)
               northern-neighbor (:north cell-neighbors)
               visited (conj tmp cell)]

           (visiting cell)

           (if (or (nil? eastern-neighbor)         ;did we reach the most eastern cell?
                   (and (some? northern-neighbor)  ;can we still move up?
                        (= 0 (rand-int 2))))       ;toss a coin
             (do
               (when (some? northern-neighbor)
                 (let [member (rand-nth visited)]
                   (when-let [northern-member (:north (neighbors member grid))]
                     (link member northern-member)
                     (wall-down member northern-member))))
               (recur [] (rest remaining)))

             (do
               (link cell eastern-neighbor)
               (wall-down cell eastern-neighbor)
               (recur visited (rest remaining)))))))

     (traverse-cells [] row))

   grid))
