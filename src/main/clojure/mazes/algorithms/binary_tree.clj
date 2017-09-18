(ns mazes.algorithms.binary-tree
  (use [mazes.grid :only (cells-from neighbors)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-emiter)])
  (require [mazes.cell :refer :all]))

(defn visit
  ([grid] (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-emiter f))

   (doseq [cell (cells-from grid)]

     (visiting cell)

     (let [neighbors (neighbors cell grid)
           candidates (filter some? [(:north neighbors)
                                     (:east neighbors)])]
       (when-not (empty? candidates)
         (let [rand-cell (rand-nth candidates)]
           (wall-down cell rand-cell)
           (link cell rand-cell)))))
   grid))
