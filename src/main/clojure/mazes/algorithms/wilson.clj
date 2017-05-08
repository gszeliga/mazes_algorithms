(ns mazes.algorithms.wilson
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (cells-from neighbors-from cell-at)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-emiter)]))

(defn visit
  ([grid]
   (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-emiter f))

   (defn loop-erased [unvisited]

     (defn build-path [current-cell path]

       (visiting current-cell)

       (if-not (contains? unvisited current-cell)
         path
         (let [rand-neighbor (->> (neighbors-from current-cell grid) vals (filter some?) rand-nth)
               rand-neighbor-pos (.indexOf path rand-neighbor)]
           (if-not (= -1 rand-neighbor-pos)
             (recur rand-neighbor (take (inc rand-neighbor-pos) path))
             (recur rand-neighbor (conj path rand-neighbor))))))

     (let [rand-cell (rand-nth unvisited)]
       (build-path rand-cell (list rand-cell))))

   (defn carve-passages [grid path]
     (doseq [[from to] (map (fn [[f t]] [(apply cell-at grid f) (apply cell-at grid t)]) (partition 2 1 path))]
       (wall-down from to)
       (link from to)))

   (defn do-visit [grid unvisited]
     (if (empty? unvisited)
       grid
       (let [path (loop-erased unvisited)]
         (carve-passages grid path)
         (recur grid (remove (set path) unvisited)))))

   (do-visit grid (cells-from grid))))
