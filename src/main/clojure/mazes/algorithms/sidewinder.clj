(ns mazes.algorithms.sidewinder
  (use [mazes.grid :only (rows-from cells-from neighbors-from)])
  (require [mazes.cell :refer :all]))

(defn visit [grid]
  (doseq [row (rows-from grid)]

    (defn traverse-cells [tmp remaining]
      (let [cell (first remaining)
            neighbors (neighbors-from cell grid)
            visited (conj tmp cell)]

        (if (or (-> neighbors :east nil?)         ;did we reach the most eastern cell?
                (and (-> neighbors :north some?)  ;can we still move up?
                     (= 0 (rand-int 2))))
          (do
            (when (-> neighbors :north some?)
              (let [member (rand-nth visited)]
                (when-let [northern-member (:north (neighbors-from member grid))]
                  (link member northern-member))))
            (recur [] (rest remaining)))

          (do
            (link cell (:east neighbors))
            (recur visited (rest remaining))))))

    (traverse-cells [] (cells-from row)))

  grid)
