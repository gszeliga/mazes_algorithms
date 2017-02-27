(ns mazes.algorithms.sidewinder
  (use [mazes.grid :only (rows-from cells-from neighbors-from)])
  (require [mazes.cell :refer :all]))

;TODO Maybe we should make it part of the grid
(defn- publisher [f]
  (fn [from to]
    (f #{(to-id from) (to-id to)})))

(defn visit
  ([grid] (visit grid (fn [e])))
  ([grid f]

   (def push->
     (publisher f))

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
                     (link member northern-member)
                     (push-> member northern-member))))
               (recur [] (rest remaining)))

             (do
               (link cell eastern-neighbor)
               (push-> cell eastern-neighbor)
               (recur visited (rest remaining)))))))

     (traverse-cells [] row))

   grid))
