(ns mazes.display.core
  (use [mazes.grid :only (rows-from neighbors n-cols n-rows cells-from)]
       [mazes.display.core-string :only (with-spaces)]
       [mazes.algorithms.events :only (poll!)])
  (require [mazes.cell :refer :all]
           [quil.core :as q :include-macros true]))

(defn- ^:private polar-coord-fn [grid size]
  (let [rows          (n-rows grid)
        canvas-size   (* 2 (* rows size))
        canvas-center (/ canvas-size 2)]
    (fn [row col]
      (let [theta        (/ (* 2 (Math/PI)) (-> grid (get row) count))
            inner_radius (* row size)
            outer_radius (* (inc row) size)
            theta_ccw    (* col theta)
            theta_cw     (* (inc col) theta)
            ax           (+ canvas-center (* inner_radius (Math/cos theta_ccw)))
            ay           (+ canvas-center (* inner_radius (Math/sin theta_ccw)))
            bx           (+ canvas-center (* outer_radius (Math/cos theta_ccw)))
            by           (+ canvas-center (* outer_radius (Math/sin theta_ccw)))
            cx           (+ canvas-center (* inner_radius (Math/cos theta_cw)))
            cy           (+ canvas-center (* inner_radius (Math/sin theta_cw)))
            dx           (+ canvas-center (* outer_radius (Math/cos theta_cw)))
            dy           (+ canvas-center (* outer_radius (Math/sin theta_cw)))]

        [[ax ay] [bx by] [cx cy] [dx dy]]))))

(defmulti ^:private walls-at 
  (fn [grid & _] (-> grid meta :type)))

(defmethod walls-at :standard
  ([grid size]
   (walls-at grid size identity identity))
  ([grid size f g]
   (fn [row col]
     ;; we need to use the opposite row because of how quil works
     (let [opposite-row (- (dec (n-rows grid)) row)
           x1           (* col size)
           y1           (* opposite-row size)
           x2           (+ x1 size)
           y2           (+ y1 size)]
       {:east  [x2 (f y1) x2 (g y2)]
        :west  [x1 (f y1) x1 (g y2)]
        :north [(f x1) y1 (g x2) y1]
        :south [(f x1) y2 (g x2) y2]}))))

(defmethod walls-at :polar
  ([grid size]
   (walls-at grid size identity identity))
  ([grid size f g]
   (let [polar-coord (polar-coord-fn grid size)]
     (fn [row col]
       (let [[[ax ay] [bx by] [cx cy] [dx dy]] (polar-coord row col)]
         {
          :cw      [cx cy dx dy]
          :ccw     [ax ay bx by]
          :outward [bx by dx dy]
          :inward  [ax ay cx cy]
          })))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Determines the center of a cell according to grid type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private cell-center
  (fn [grid _] (-> grid meta :type)))

(defmethod cell-center :standard [grid size]
  (fn [row col]
    ;;we need to use the opposite row because of how quil works
    (let [opposite-row (- (dec (n-rows grid)) row)
          x1           (* col size)
          y1           (* opposite-row size)
          center-x     (+ x1 (/ size 2))
          center-y     (+ y1 (/ size 2))]
      [center-x center-y])))

(defmethod cell-center :polar [grid size]
  (let  [polar-coord (polar-coord-fn grid size)]
    (fn [row col]
      (let [[mx my] (apply map + (polar-coord row col))]
        ;; Use use the 'Finite set of points' method
        ;; https://en.wikipedia.org/wiki/Centroid#Of_a_finite_set_of_points
        [(/ mx 4) (/ my 4)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         Draws a Grid as string (:standard type only)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn stringify
  ([grid]
   (stringify grid with-spaces))
  ([grid render-cell]
   (defn cell->str [row-line cell]
     (let [[row-line-top row-line-bottom] row-line
           neighbors                      (neighbors cell grid)
           east-cell                      (:east neighbors)
           south-cell                     (:south neighbors)]

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  Inferes proper canvas size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private canvas-size 
  (fn [grid _] (-> grid meta :type)))

(defmethod canvas-size :polar [grid size]
  (let [infered-size (inc (* 2 (* (n-rows grid) size)))]
    [infered-size infered-size]))

(defmethod canvas-size :standard  [grid size]
  [(* (n-cols grid) size)
   (* (n-rows grid) size)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Draws a Grid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti ^:private draw-grid 
  (fn [grid & _] (-> grid meta :type)))

(defmethod draw-grid :polar [grid size show-links]

  (let [rows          (n-rows grid)
        canvas-size   (* 2 (* rows size))
        canvas-center (/ canvas-size 2)
        center-in     (cell-center grid size)
        polar-coord   (polar-coord-fn grid size)]

    (q/ellipse canvas-center canvas-center canvas-size canvas-size)

    (doseq [cell (filter #(-> (:row %) zero? not)
                         (cells-from grid))]

      (let [cell-ngh                          (neighbors cell grid)
            row                               (:row cell)
            col                               (:column cell)
            [[ax ay] [bx by] [cx cy] [dx dy]] (polar-coord row col)]

        ;; (q/with-fill [255 0 0]
        ;;   (q/text (str "[" (:row cell) "-" (:column cell) "]") mx my))

        (when-not (and show-links (linked? cell (:inward cell-ngh)))
          (q/line ax ay cx cy))

        (when-not (and show-links (linked? cell (:cw cell-ngh)))
          (q/line cx cy dx dy))))))

(defmethod draw-grid :standard [grid size show-links]

  (def walls-from #((walls-at grid size) (:row %) (:column %)))

  (defn draw-cell [cell]
    (let [walls (walls-from cell)]
      (doseq [[orientation neighbor] (neighbors cell grid)]
        (when (or (nil? neighbor)
                  (not show-links)
                  (not (linked? cell neighbor)))
          (apply q/line (orientation walls))))))

  (doseq [cell (cells-from grid)]
    (draw-cell cell)))

(defn draw
  [grid & {:keys [size stroke with-path]
           :or   {size 10 stroke 3 with-path nil}}]

  (def center-in #((cell-center grid size) %1 %2))

  (defn setup []
    (q/background 255)
    (draw-grid grid size true))

  (defn draw-path []
    (when-not (nil? with-path)
      (q/stroke-weight stroke)
      (doseq [[from to] (partition 2 1 with-path)]
        (q/with-stroke [255 0 0]
          (apply q/line (concat (apply center-in from) (apply center-in to)))))))

  (q/defsketch sample-maze
    :size (canvas-size grid size)
    :setup setup
    :draw draw-path))

(defn animate!
  [grid events & {:keys [size speed stroke]
                  :or   {size 10 speed 10 stroke 5}}]

  (def point-offset (int (Math/ceil (/ stroke 2))))
  (def walls-from #(apply (walls-at grid size) %))
  (def walls-to-tear-down-from #(apply (walls-at grid size (partial + point-offset) (fn [v] (- v point-offset))) %))

  (defn setup []
    (q/frame-rate speed)
    (q/background 255)
    (draw-grid grid size false))

  (defn as-wall [side-a side-b]
    (let [neighbors-a      (apply #(neighbors %1 %2 grid :all) side-a)
          [at-orientation] (keys (filter #(when-some [cell (val %)]
                                            (= (to-id cell) side-b)) neighbors-a))]
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
    :size (canvas-size grid size) 
    :setup setup
    :draw (do-draw (ref nil))))


