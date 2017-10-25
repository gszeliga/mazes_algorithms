(ns mazes.display.core-sigma)

(defn sigma-coord-fn [size]

  (let [a-size (/ size 2.0)
        b-size (-> (Math/sqrt 3)
                   (* size)
                   (/ 2.0))
        width  (* size 2)
        height (* b-size 2)]
    
    (fn [row col]
      (let [cx   (+ size (* col a-size 3))
            cy   (+ (if (odd? col) (* 2 b-size) b-size)
                    (* row height))
            x_fw (int (- cx size))
            x_nw (int (- cx a-size))
            x_ne (int (+ cx a-size))
            x_fe (int (+ cx size))
            y_n  (int (- cy b-size))
            y_m  (int cy)
            y_s  (int (+ cy b-size))]
        
        [[x_fw y_m] [x_nw y_n] [x_ne y_n]
         [x_fe y_m] [x_ne y_s] [x_nw y_s]]))))
