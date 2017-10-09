(ns mazes.dijkstra.longest-path
  (use [mazes.dijkstra.distances]
       [mazes.dijkstra.path]))

(defn- greatest-distance-in [distances]
  (apply max-key second (seq distances)))

(defn longest-path-in [grid]

  (let [[most-distant-cell, _] (greatest-distance-in (distances-from grid 1 1))
        distances (apply distances-from grid most-distant-cell)
        [goal, _] (greatest-distance-in distances)]

    (path-to grid most-distant-cell goal)))
