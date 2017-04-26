(ns mazes.display.core
  (use [mazes.grid :only (rows-from neighbors-from n-cols n-rows cells-from)]
       [mazes.display.core-string :only (with-spaces)]
       [mazes.algorithms.events :only (poll!)])
  (require [mazes.cell :refer :all] :reload
           [quil.core :as q :include-macros true])
  (:gen-class))

(defn- walls-at
  ([grid size]
   (walls-at grid size identity identity))
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

(defn- cell-center
  [grid size]
  (fn [row col]
    (let [opposite-row (- (dec (n-rows grid)) row) ;we need to use the opposite row because of how quil works
          x1 (* col size)
          y1 (* opposite-row size)
          center-x (+ x1 (/ size 2))
          center-y (+ y1 (/ size 2))]
      [center-x center-y])))

(defn stringify
  ([grid]
   (stringify grid with-spaces))
  ([grid render-cell]
   (defn cell->str [row-line cell]
     (let [[row-line-top row-line-bottom] row-line
           neighbors (neighbors-from cell grid)
           east-cell (:east neighbors)
           south-cell (:south neighbors)]

       [(str row-line-top (render-cell (to-id cell))
             (if (and (some? east-cell)
                      (linked? cell east-cell))
               " "
               "|"))

        (str row-line-bottom (if (and (some? south-cell)
                                      (linked? cell south-cell))
                               "   "
                               "---") "+")]))

   (defn row->str [grid-as-string row]
     (str (apply str grid-as-string
                 (interpose \newline (reduce cell->str ["|" "+"] row)))
          \newline))

   (reduce row->str
           (str "+" (apply str (repeat (n-cols grid) "---+")) \newline)
           (reverse (rows-from grid)))))

(defn- draw-grid
  [grid & {:keys [size] :or {size 10}}]

  (def walls-from #((walls-at grid size) (:row %) (:column %)))

  (defn draw-cell [cell]
    (let [walls (walls-from cell)]
      (doseq [[orientation neighbor] (neighbors-from cell grid)]
        (when (or (nil? neighbor)
                  (not (linked? cell neighbor)))
          (apply q/line (orientation walls))))))

  (doseq [cell (cells-from grid)]
    (draw-cell cell)))

(defn draw
  [grid & {:keys [size stroke with-path]
           :or {size 10 stroke 3 with-path nil}}]

  (def center-of #((cell-center grid size) %1 %2))

  (defn setup []
    (q/background 255)
    (draw-grid grid :size size))

  (defn draw-path []
    (when-not (nil? with-path)
      (q/stroke-weight stroke)
      (doseq [[from to] (partition 2 1 with-path)]
        (q/with-stroke [255 0 0]
          (apply q/line (concat (apply center-of from) (apply center-of to)))))))

  (q/defsketch sample-maze
    :size [(* (n-cols grid) size)
           (* (n-rows grid) size)]
    :setup setup
    :draw draw-path))

(defn animate!
  [grid events & {:keys [size speed stroke]
                  :or {size 10 speed 10 stroke 5}}]

  (def point-offset (int (Math/ceil (/ stroke 2))))
  (def walls-from #(apply (walls-at grid size) %))
  (def walls-to-tear-down-from #(apply (walls-at grid size (partial + point-offset) (fn [v] (- v point-offset))) %))

  (defn setup []
    (q/frame-rate speed)
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
    :size [(* (n-cols grid) size)
           (* (n-rows grid) size)]
    :setup setup
    :draw (do-draw (ref nil))))
