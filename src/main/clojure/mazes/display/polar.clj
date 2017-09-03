(ns mazes.display.polar
  (require [mazes.grid :refer (n-rows n-cols cells-from)]
           [quil.core :as q :include-macros true]))

(defn draw [grid & {:keys [size]
                    :or {size 10}}]

  (let [cols (n-cols grid)
        rows (n-rows grid)
        theta (/ (* 2 (Math/PI)) cols)
        canvas-size (* 2 (* rows size))
        canvas-center (/ canvas-size 2)]

    (defn draw-grid [grid]

      (q/ellipse canvas-center canvas-center canvas-size canvas-size)

      (doseq [cell (cells-from grid)]

        (let [inner_radius (* (:row cell) size)
              outer_radius (* (inc (:row cell)) size)
              theta_ccw (* (:column cell) theta)
              theta_cw (* (inc (:column cell)) theta)
              ax (+ canvas-center (* inner_radius (Math/cos theta_ccw)))
              ay (+ canvas-center (* inner_radius (Math/sin theta_ccw)))
              bx (+ canvas-center (* outer_radius (Math/cos theta_ccw)))
              by (+ canvas-center (* outer_radius (Math/sin theta_ccw)))
              cx (+ canvas-center (* inner_radius (Math/cos theta_cw)))
              cy (+ canvas-center (* inner_radius (Math/sin theta_cw)))
              dx (+ canvas-center (* outer_radius (Math/cos theta_cw)))
              dy (+ canvas-center (* outer_radius (Math/sin theta_cw)))]

          (q/line ax ay cx cy)
          (q/line cx cy dx dy))))

    (defn setup []
      (q/background 255)
      (draw-grid grid))

    (q/defsketch sample-maze
      :size [(inc canvas-size)
             (inc canvas-size)]
      :setup setup
      :draw (fn []))))
