(ns mazes.algorithms.aldous-broder
  (use [mazes.grid :only (cells-from neighbors-from rand-cell-at n-cells)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-emiter)])
  (require [mazes.cell :refer :all]))

(defn visit
  ([grid]
   (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-emiter f))

   (defn visit-next [grid current-cell remaining]

     (visiting current-cell)

     (if (zero? remaining)
       grid
       (let [neighbor (->> (neighbors-from current-cell grid) vals (filter some?) rand-nth)]

         (if (empty? (links neighbor))
           (do
             (link current-cell neighbor)
             (wall-down current-cell neighbor)
             (recur grid neighbor (dec remaining)))
           (recur grid neighbor remaining)))))

   (visit-next grid (rand-cell-at grid) (-> grid (n-cells) (dec)))))
