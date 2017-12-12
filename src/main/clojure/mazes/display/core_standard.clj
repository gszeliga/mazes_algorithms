(ns mazes.display.core-standard)

(defn standard-vertex-fn [size]
  (fn [row col]
    (let [x1 (* col size)
          y1 (* row size)
          x2 (+ x1 size)
          y2 (+ y1 size)]
      [x1 y1 x2 y2])))

(defn standard-coord-fn 
  ([size] 
   (standard-coord-fn size 0))
  ([size inset]
   (let [vertex-fn (standard-vertex-fn size)]
     (fn [row col]
       (let [[x1 y1 x2 y2 :as default-vtx] (vertex-fn row col)
             x1-in                         (+ x1 inset)
             y1-in                         (+ y1 inset)
             x2-in                         (- x2 inset)
             y2-in                         (- y2 inset)]
         {:default default-vtx
          :inset   [x1-in y1-in x2-in y2-in]})))))

