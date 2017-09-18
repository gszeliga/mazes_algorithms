(ns mazes.algorithms.wilson
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (cells-from neighbors cell-at)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-id-emiter)]))

(defn visit
  ([grid]
   (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-id-emiter f))

   (defn loop-erased [unvisited]

     (defn build-path [current-cell-id path]

       (visiting current-cell-id)

       (if-not (contains? unvisited current-cell-id)
         path
         (let [[row,col] current-cell-id
               rand-neighbor (->> (neighbors row col grid :present) vals rand-nth to-id)
               rand-neighbor-pos (.indexOf path rand-neighbor)]

           ;Is it a loop?
           (if-not (= -1 rand-neighbor-pos)

             ; Why (apply vector...)? Because we need to preserve the collection type, and therefore, (conj) behaviour
             (recur rand-neighbor (apply vector (take (inc rand-neighbor-pos) path)))
             (recur rand-neighbor (conj path rand-neighbor))))))

     (let [rand-cell (rand-nth (vec unvisited))]
       (build-path rand-cell [rand-cell])))

   (defn carve-passages [grid path]
     (doseq [[from to] (map (fn [[f t]] [(apply cell-at grid f) (apply cell-at grid t)]) (partition 2 1 path))]
       (wall-down from to)
       (link from to)))

   (defn do-visit [grid unvisited]
     (if (empty? unvisited)
       grid
       (let [path (loop-erased unvisited)]
         (carve-passages grid path)
         (recur grid (set (remove (set path) unvisited))))))

   (let [unvisited (set (map to-id (cells-from grid)))
         rand-cell (rand-nth (vec unvisited))]
     (do-visit grid (disj unvisited rand-cell)))))
