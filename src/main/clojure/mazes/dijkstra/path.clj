(ns mazes.dijkstra.path
  (require [mazes.cell :refer :all])
  (use [mazes.dijkstra.distances]
       [mazes.grid :only (cell-at)]))

(defn path-to
  ([grid from to]

   (def goal from)
   (def distances (distances-from grid (apply cell-at grid from)))

   (defn is-forward-step [current-id neighbor-id]
     (< (get distances neighbor-id)
        (get distances current-id)))

   (defn find-path [breadcrumbs current]
     (let [path (conj breadcrumbs current)]
       (if-not (= current goal)
         (let [step-forward? (partial is-forward-step current)
               forward-step (->> (apply cell-at grid current) (links) (filter step-forward?) (first))]
           (recur path forward-step))

         path)))

   (find-path '() to)))
