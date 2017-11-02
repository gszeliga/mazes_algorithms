(ns mazes.display.core-triangle)

(defn triangle-coord-fn [size]
  (let [half-width  (/ size 2.0)
        height      (/ (* size (Math/sqrt 3)) 2.0)
        half-height (/ height 2.0)]

    (fn [row col]
      (let [upright? (even? (+ row col))
            cx       (+ half-width (* half-width col))
            cy       (+ half-height (* height row))
            west_x   (int (- cx half-width))
            mid_x    (int cx)
            east_x   (int (+ cx half-width))
            apex_y   (int ((if upright? - +) cy half-height))
            base_y   (int ((if upright? + -) cy half-height))]

        [[west_x base_y] [mid_x apex_y] [east_x base_y]]))))
