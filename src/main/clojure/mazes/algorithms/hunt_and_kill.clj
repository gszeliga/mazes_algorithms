(ns mazes.algorithms.hunt-and-kill
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (cells-from neighbors-from cell-at rand-cell-at)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-emiter)]))

(defn visit
  ([grid]
   (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-emiter f))

   (defn hunt-unvisited [grid]

     (defn do-hunt [grid non-visited-cells]

       (when-let [cell (first non-visited-cells)]
         (if-let [visited-neighbor (rand-nth (filter #(not-empty (links %)) (->> (neighbors-from cell grid) vals (filter some?))))]
           (do
             (link cell visited-neighbor)
             (wall-down cell visited-neighbor)
             cell)
           (recur grid (rest non-visited-cells)))))

     (do-hunt grid (filter #(empty? (links %)) (cells-from grid))))

   (defn walk [grid current-cell]
     (if (some? current-cell)
       (let [_ (visiting current-cell)
             neighbors (->> (neighbors-from current-cell grid) vals (filter some?))
             unvisited-neighbors (filter #(empty? (links %)) neighbors)]
         (if-let [unvisited (rand-nth unvisited-neighbors)]
           (do (link current-cell unvisited)
               (wall-down current-cell unvisited)
               (recur grid  unvisited))
           (recur grid (hunt-unvisited grid))))
       grid))

   (walk grid (rand-cell-at grid))))
