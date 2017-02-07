(ns mazes.algorithms.binary-tree
  (use [mazes.grid :only (cells-from neighbors-from)] :reload)
  (require [mazes.cell :refer :all])
  (:gen-class))

(defn do-apply [grid]
  (doseq [cell (cells-from grid)]
    (let [neighbors (neighbors-from cell grid)
          candidates (filter some? [(:north neighbors)
                                    (:east neighbors)])]
      (when-not (empty? candidates)
        (link cell (rand-nth candidates) true))))
  grid)