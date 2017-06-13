(ns mazes.algorithms.hunt-and-kill
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (cells-from neighbors-available neighbors-not-linked cell-at rand-cell-at)])
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
         (if-let [visited-neighbor  (rand-nth (not-empty (filter #(not-empty (links %)) (neighbors-available cell grid))))]
           (do (link cell visited-neighbor)
               (wall-down cell visited-neighbor)
               cell)
           (recur grid (rest non-visited-cells)))))

     (do-hunt grid (filter #(empty? (links %)) (cells-from grid))))

   (defn walk [grid current-cell]
     (if (some? current-cell)
       (let [_ (visiting current-cell)]
         (if-let [unvisited (rand-nth (not-empty (neighbors-not-linked current-cell grid)))]
           (do (link current-cell unvisited)
               (wall-down current-cell unvisited)
               (recur grid  unvisited))
           (recur grid (hunt-unvisited grid))))
       grid))

   (walk grid (rand-cell-at grid))))
