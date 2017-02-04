(ns mazes.algorithms.binary-tree
  (use [mazes.grid :only (cells-from neighbors-from)] :reload)
  (require [mazes.cell :refer :all])
  (:gen-class))

(defn do-apply [grid]
  (for [cell (cells-from grid)
        neighbors (neighbors-from grid cell)]
    (link cell
          (rand-nth (filter some? (vector (:north neighbors)
                                          (:east neighbors)))) true)))
