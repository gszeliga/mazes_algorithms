(ns mazes.display.core-standard)

(defn standard-coord-fn [size]
  (fn [row col]
    (let [x1 (* col size)
          y1 (* row size)
          x2 (+ x1 size)
          y2 (+ y1 size)]
      [[x1 y1] [x1 y2] [x2 y2] [x2 y1]])))

