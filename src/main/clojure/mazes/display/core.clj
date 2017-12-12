(ns mazes.display.core
  (use [mazes.grid :only (rows-from neighbors n-cols n-rows cells-from)]
       [mazes.display.core-string :only (with-spaces)]
       [mazes.algorithms.events :only (poll!)])
  (require [mazes.cell :refer :all]
           [quil.core :as q :include-macros true]
           [mazes.display.core-polar :refer (polar-coord-fn)]
           [mazes.display.core-sigma :refer (sigma-coord-fn)]
           [mazes.display.core-triangle :refer (triangle-coord-fn)]
           [mazes.display.core-standard :refer (standard-coord-fn)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Lists coordinates from all walls in a cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private walls-at 
  (fn [grid & _] (-> grid meta :type)))

;; (defmethod walls-at :standard
;;   ([grid size inset]
;;    (let [standard-coord (standard-coord-fn size inset)]
;;      (fn [row col]
;;        (let [[ws wn en es]  (standard-coord row col)]
;;          {:east  (into [] (concat es en))
;;           :west  (into [] (concat ws wn))
;;           :north (into [] (concat wn en))
;;           :south (into [] (concat ws es))})))))

(defmethod walls-at :standard
  ([grid size inset]
   (let [coords-fn (standard-coord-fn size inset)]
     (fn [cell]
       (let [{[x1 y1 x2 y2] :default, inset :inset} (coords-fn (:row cell)         
                                                               (:column cell))]
         (if (not-empty inset)
           (let [[x1-in y1-in x2-in y2-in] inset]
             {:linked 
              {:east  [[x2-in y2-in x2 y2-in] [x2-in y1-in x2 y1-in]]
               :west  [[x1 y2-in x1-in y2-in] [x1 y1-in x1-in y1-in]]
               :north [[x1-in y2 x1-in y2-in] [x2-in y2 x2-in y2-in]]
               :south [[x1-in y1-in x1-in y1] [x2-in y1-in x2-in y1]]}
              :not-linked 
              {:east  [[x1-in y2-in x2-in y1-in]]
               :west  [[x1-in y2-in x1-in y1-in]]
               :north [[x1-in y2-in x2-in y2-in]]
               :south [[x1-in y1-in x2-in y1-in]]}})
           {:not-linked 
            {:east  [[x2 y2 x2 y1]]
             :west  [[x1 y1 x1 y2]]
             :north [[x1 y2 x2 y2]]
             :south [[x1 y1 x2 y1]]}}))))))

(defmethod walls-at :polar
  ([grid size inset]
   (let [polar-coord (polar-coord-fn grid size)]
     (fn [row col]
       (let [[[ax ay] [bx by] [cx cy] [dx dy]] (polar-coord row col)]
         {
          :cw      [cx cy dx dy]
          :ccw     [ax ay bx by]
          :outward [bx by dx dy]
          :inward  [ax ay cx cy]})))))

(defmethod walls-at :sigma
  ([grid size inset]
   (let [sigma-coord (sigma-coord-fn size)]
     (fn [row col]
       (let [[fwm nwn nen fem nes nws] (sigma-coord row col)]
         {
          :northwest (into [] (concat fwm nwn))
          :north     (into [] (concat nwn nen))
          :northeast (into [] (concat nen fem))
          :southeast (into [] (concat fem nes))
          :south     (into [] (concat nes nws))
          :southwest (into [] (concat nws fwm))})))))

(defmethod walls-at :triangle
  [grid size inset]
  (let [triangle-coord (triangle-coord-fn size)]
    (fn [row col]
      (let [upright?        (even? (+ row col))
            [west mid east] (triangle-coord row col)]
        {:east  (into [] (concat mid east))
         :north (when (not upright?) (into [] (concat east west)))
         :west  (into [] (concat west mid))
         :south (when upright? (into [] (concat east west)))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Determines the center of a cell according to grid type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ^:private cell-center-fn
  [grid size coords-fn]
  (fn [row col]
    (let [coords      (coords-fn row col)
          coords-n    (count coords)
          [sumx sumy] (apply map + coords)]
      ;; Use use the 'Finite set of points' method
      ;; https://en.wikipedia.org/wiki/Centroid#Of_a_finite_set_of_points
      [(/ sumx coords-n) (/ sumy coords-n)])))

(defmulti ^:private cell-center
  (fn [grid _] (-> grid meta :type)))

(defmethod cell-center :standard
  [grid size]
  (cell-center-fn grid size (standard-coord-fn size)))

(defmethod cell-center :polar
  [grid size]
  (cell-center-fn grid size (polar-coord-fn grid size)))

(defmethod cell-center :sigma
  [grid size]
  (cell-center-fn grid size (sigma-coord-fn size)))

(defmethod cell-center :triangle
  [grid size]
  (cell-center-fn grid size (triangle-coord-fn size)))

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

(defmethod canvas-size :polar
  [grid size]
  (let [infered-size (inc (* 2 (* (n-rows grid) size)))]
    [infered-size infered-size]))

(defmethod canvas-size :sigma
  [grid size]
  (let [a-size (/ size 2.0)
        b-size (/ (* size (Math/sqrt 3)) 2.0)
        width  (+ a-size 0.5 (* 3 a-size (n-cols grid)))
        height (+ b-size 0.5 (* b-size 2 (n-rows grid)))]
    [width height]))

(defmethod canvas-size :triangle
  [grid size]
  (let [tri-height (-> size
                       (* (Math/sqrt 3))
                       (/ 2.0))
        width      (-> (inc (n-cols grid))
                       (* size)
                       (/ 2.0)
                       int)
        height     (-> (n-rows grid)
                       (* tri-height)
                       int)]
    [(inc width) (inc height)]))

(defmethod canvas-size :standard
  [grid size]
  [(* (n-cols grid) size)
   (* (n-rows grid) size)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Draws a Grid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private draw-grid 
  (fn [grid & _] (-> grid meta :type)))

(defmethod draw-grid :polar
  [grid size show-links]
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

        (when-not (and show-links 
                       (linked? cell (:inward cell-ngh)))
          (q/line ax ay cx cy))

        (when-not (and show-links 
                       (linked? cell (:cw cell-ngh)))
          (q/line cx cy dx dy))))))

(defmethod draw-grid :default
  [grid size show-links]
  (let [walls-from-fn (walls-at grid size 1)]
    (defn draw-cell [cell]
      (let [walls (walls-from-fn cell)]
        (doseq [[orientation ngh-cell] (neighbors cell grid)]
          (when (some? ngh-cell)
            (let [state (if (or (not show-links)
                                (not (linked? cell ngh-cell)))
                          :not-linked
                          :linked)]
              (doseq [wall (get-in walls [state orientation] [])] 
                (apply q/line wall)))))))

    (doseq [cell (cells-from grid)]
      (draw-cell cell))))

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
          (apply q/line (concat (apply center-in from)
                                (apply center-in to)))))))

  (q/defsketch sample-maze
    :size (canvas-size grid size)
    :settings #(q/smooth 8)
    :setup setup
    :draw draw-path))

(defn animate!
  [grid events & {:keys [size speed stroke]
                  :or   {size 10 speed 10 stroke 1}}]

  (def walls-from 
    #(apply (walls-at grid size) %))

  (defn setup []
    (q/frame-rate speed)
    (q/background 255)
    (draw-grid grid size false))

  (defn as-wall [side-a side-b]
    (let [neighbors-a      (apply #(neighbors %1 %2 grid :all) side-a)
          [at-orientation] (keys (filter #(when-some [cell (val %)]
                                            (= (to-id cell) side-b))
                                         neighbors-a))]
      (at-orientation (walls-from side-a))))

  (defn do-draw [previous-wall]
    (fn []
      (q/stroke-cap :round)
      (q/stroke-weight stroke)

      (doseq [wall (->> events
                        (poll! :wall-down)
                        (map #(apply as-wall (:values %))))]
        (dosync
         (when-let [p-wall (deref previous-wall)]
           (q/stroke 255 255 255)
           (apply q/line p-wall))
         (q/stroke 255 0 0)
         (apply q/line (ref-set previous-wall wall))))))

  (q/defsketch sample-maze
    :size (canvas-size grid size)
    :settings #(q/smooth 8) 
    :setup setup
    :draw (do-draw (ref nil))))


