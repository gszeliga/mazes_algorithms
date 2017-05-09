(ns mazes.algorithms.wilson
  (require [mazes.cell :refer :all])
  (use [mazes.grid :only (cells-from neighbors-from cell-at)])
  (use [mazes.algorithms.events :only (wall-down-emiter visiting-cell-id-emiter)]))

(defn visit
  ([grid]
   (visit grid (fn [_])))
  ([grid f]

   (def wall-down
     (wall-down-emiter f))

   (def visiting
     (visiting-cell-id-emiter f))

   ;Creo que deberia pasar el random-cell junto con el unvisited actualizado para evitar el infinite loop
   (defn loop-erased [unvisited]

     (defn build-path [current-cell path]

       (visiting current-cell)

       (prn (str "path=" path))

       (if-not (contains? unvisited current-cell)
         path
         (let [rand-neighbor (->> (neighbors-from (first current-cell) (second current-cell) grid) vals (filter some?) rand-nth to-id)
               rand-neighbor-pos (.indexOf path rand-neighbor)]
           (if-not (= -1 rand-neighbor-pos)
             (recur rand-neighbor (seq (take (inc rand-neighbor-pos) path)))
             (recur rand-neighbor (conj path rand-neighbor))))))

     (let [rand-cell (rand-nth (vec unvisited))]
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
         (recur grid (set (remove (set path) unvisited))))))

   (do-visit grid (set (map to-id (cells-from grid))))))
