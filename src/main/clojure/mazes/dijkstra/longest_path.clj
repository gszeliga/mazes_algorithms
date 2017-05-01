(ns mazes.dijkstra.longest-path
  (use [mazes.dijkstra.distances]
       [mazes.dijkstra.path]))

(defn- greatest-distance [distances]
  (apply max-key second (seq distances)))

(defn longest-path [grid]

  (let [[most-distant-cell, distance] (greatest-distance (distances-from grid 0 0))
        distances (distances-from grid most-distant-cell)
        [goal, distance] (greatest-distance distances)]

    (path-to grid most-distant-cell goal)))
