(ns mazes.algorithms.sidewinder
  (use [mazes.grid :only (rows-from cells-from neighbors-from)])
  (require [mazes.cell :refer :all]))

(defn visit [grid]
  (doseq [row (rows-from grid)]

    (defn traverse-cells [tmp remaining]

      (if-let [cell (first remaining)]

        (let [neighbors (neighbors-from cell grid)
              eastern-neighbor (:east neighbors)
              northern-neighbor (:north neighbors)
              visited (conj tmp cell)]

          (if (or (nil? eastern-neighbor)         ;did we reach the most eastern cell?
                  (and (some? northern-neighbor)  ;can we still move up?
                       (= 0 (rand-int 2))))       ;toss a coin
            (do
              (when (some? northern-neighbor)
                (let [member (rand-nth visited)]
                  (when-let [northern-member (:north (neighbors-from member grid))]
                    (link member northern-member true))))
              (recur [] (rest remaining)))

            (do
              (link cell eastern-neighbor true)
              (recur visited (rest remaining)))))))

    (traverse-cells [] row))

  grid)