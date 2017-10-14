(ns mazes.display.core-polar
  (require [mazes.grid :refer (n-rows)]))

(defn polar-coord-fn [grid size]
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
