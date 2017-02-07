(ns mazes.display
  (use [mazes.grid :only (rows-from neighbors-from n-cols)])
  (require [mazes.cell :refer :all]))

(defn as-str [grid]
  (reduce (fn [output row]
            (let [top "|"
                  bottom "+"]

              (str (apply str output (interpose \newline (reduce (fn [boundaries cell]
                                                                   (let [[cell_top cell_bottom] boundaries
                                                                         neighbors (neighbors-from cell grid)]
                                                                     [(str cell_top "   " (if (linked? cell (:east neighbors)) " " "|"))
                                                                      (str cell_bottom (if (linked? cell (:south neighbors)) "   " "---") "+")]))
                                                                 [top bottom]
                                                                 row))) \newline)))

          (str "+" (apply str (repeat (n-cols grid) "---+")) \newline)
          (rows-from grid)))
