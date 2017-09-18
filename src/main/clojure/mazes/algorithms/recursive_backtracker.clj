(ns mazes.algorithms.recursive-backtracker
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (rand-cell-at neighbors)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-emiter)]))

(defn visit
  ([grid]
   (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-emiter f))

   (defn walk [grid visited]
     (if (empty? visited)
       grid
       (let [current-cell (first visited)
             _ (visiting current-cell)]
         (if-let [neighbor (->> (neighbors current-cell grid :not-linked) vals not-empty rand-nth)]
           (do
             (wall-down current-cell neighbor)
             (link current-cell neighbor)
             (recur grid (conj visited neighbor)))
           (recur grid (rest visited))))))

   (let [rand-cell (rand-cell-at grid)]
     (walk grid (list rand-cell)))))
