(ns mazes.display
  (use [mazes.grid :only (rows-from
                          neighbors-from
                          n-cols
                          n-rows
                          cells-from)]
       [mazes.algorithms.events :only (poll!)])

  (require [mazes.cell :refer :all] :reload
           [quil.core :as q :include-macros true])
  (:gen-class))

(defn stringify [grid]

  (defn display-row-cell [row-line cell]
    (let [[row-line-top row-line-bottom] row-line
          neighbors (neighbors-from cell grid)
          east-cell (:east neighbors)
          south-cell (:south neighbors)]

      [(str row-line-top "   " (if (and (some? east-cell)
                                        (linked? cell east-cell))
                                 " "
                                 "|"))

       (str row-line-bottom (if (and (some? south-cell)
                                     (linked? cell south-cell))
                              "   "
                              "---") "+")]))

  (defn display-grid-row [grid-as-string row]
    (str (apply str grid-as-string
                (interpose \newline (reduce display-row-cell ["|" "+"] row)))
         \newline))

  (reduce display-grid-row
          (str "+" (apply str (repeat (n-cols grid) "---+")) \newline)
          (reverse (rows-from grid))))

(defn- walls-at
  ([grid size] (walls-at grid size identity identity))
  ([grid size f g]
   (fn [row col]
     (let [opposite-row (- (dec (n-rows grid)) row) ;we need to use the opposite row because of how quil works
           x1 (* col size)
           y1 (* opposite-row size)
           x2 (+ x1 size)
           y2 (+ y1 size)]
       {:east [x2 (f y1) x2 (g y2)]
        :west [x1 (f y1) x1 (g y2)]
        :north [(f x1) y1 (g x2) y1]
        :south [(f x1) y2 (g x2) y2]}))))

(defn draw
  ([grid] (draw 40 grid))
  ([cell-size grid]

   (defn setup []
     (q/background 255))

   (def walls-from #((walls-at grid cell-size) (:row %) (:column %)))

   (defn plot-cell-with-lines [cell]
     (let [walls (walls-from cell)]
       (doseq [[orientation neighbor] (neighbors-from cell grid)]
         (when (or (nil? neighbor)
                   (not (linked? cell neighbor)))
           (apply q/line (orientation walls))))))

   (defn do-draw []
     (doseq [cell (cells-from grid)]
       (plot-cell-with-lines cell)))

   (q/defsketch sample-maze
     :size [(* (n-cols grid) cell-size)
            (* (n-rows grid) cell-size)]
     :setup setup
     :draw do-draw)))

(defn animate!
  ([events cell-size grid]
   (animate! events cell-size 10 grid))
  ([events cell-size speed grid]
   (animate! events cell-size speed 5 grid))
  ([events cell-size speed stroke grid]

   (def adjustment (int (Math/ceil (/ stroke 2))))
   (def walls-from #(apply (walls-at grid cell-size) %))
   (def walls-to-tear-down-from #(apply (walls-at grid cell-size (partial + adjustment) (fn [v] (- v adjustment))) %))

   (defn setup []
      ; draw will be called 60 times per second
     (q/frame-rate speed)
      ; set background to white colour only in the setup
      ; otherwise each invocation of 'draw' would clear sketch completely
     (q/background 255)

     (doseq [wall  (mapcat identity (->> grid cells-from (map to-id) (map walls-from) (map vals)))]
       (apply q/line wall)))

   (defn as-wall [side-a side-b]
     (let [neighbors (apply #(neighbors-from %1 %2 grid) side-a)
           [at-orientation] (keys (filter #(when-some [cell (val %)]
                                             (= (to-id cell) side-b)) neighbors))]
       (at-orientation (walls-to-tear-down-from side-a))))

   (defn do-draw [previous-wall]
     (fn []
       (q/stroke-cap :project)
       (q/stroke-weight stroke)

       (doseq [wall (->> events (poll! :wall-down) (map #(apply as-wall (:values %))))]
         (dosync
          (when-let [p-wall (deref previous-wall)]
            (q/with-stroke [255 255 255]
              (apply q/line p-wall)))
          (q/with-stroke [255 0 0]
            (apply q/line (ref-set previous-wall wall)))))))

   (q/defsketch sample-maze
     :size [(* (n-cols grid) cell-size)
            (* (n-rows grid) cell-size)]
     :setup setup
     :draw (do-draw (ref nil)))))

;fun mode: https://github.com/quil/quil/wiki/Functional-mode-%28fun-mode%29
;http://quil.info/sketches/show/example_tree
