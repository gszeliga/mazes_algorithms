(ns mazes.algorithms.binary-tree
  (use [mazes.grid :only (cells-from neighbors-from)])
  (require [mazes.cell :refer :all]))

(defn visit [grid]
  (doseq [cell (cells-from grid)]
    (let [neighbors (neighbors-from cell grid)
          candidates (filter some? [(:north neighbors)
                                    (:east neighbors)])]
      (when-not (empty? candidates)
        (link cell (rand-nth candidates) true))))
  grid)
