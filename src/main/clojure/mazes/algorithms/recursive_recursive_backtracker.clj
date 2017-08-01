(ns mazes.algorithms.recursive-recursive-backtracker
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

   (defn walk [grid current-cell]
     (let [_ (visiting current-cell)]
       (if-let [neighbor (rand-nth (not-empty (neighbors current-cell grid :not-linked)))]
         (do
           (wall-down current-cell neighbor)
           (link current-cell neighbor)
           ;FIXME it wil blow the stack at one point
           (walk grid neighbor)
           (recur grid current-cell))
         grid)))

   (let [rand-cell (rand-cell-at grid)]
     (walk grid rand-cell))))
