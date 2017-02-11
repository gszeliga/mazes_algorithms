(ns mazes.algorithms.sidewinder
  (use [mazes.grid :only (rows-from cells-from neightbors-from)])
  (require [mazes.cell :refer:all]))

(defn visit [grid]
  (doseq [row (rows-from grid)]
    ;recursion sounds like a better fit since we need to short-circuit when row's closed out
    (reduce (fn [visited cell]) [] row)))
