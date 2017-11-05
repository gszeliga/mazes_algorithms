(ns mazes.braid
  (:require [mazes.grid :refer (cells-from neighbors)]
            [mazes.cell :refer (links link linked?)]))

(defn braid [p grid]

  (doseq [current-cell (->> (cells-from grid :dead-ends)
                            (filter #(->> % links count (= 1)))
                            (filter (fn [_] (> (rand) p))))]

    (when-let [candidates (->> (neighbors current-cell grid :dead-ends)
                               vals
                               (filter #(->> % (linked? current-cell) not))
                               not-empty)]
      (link current-cell (rand-nth candidates)))))
