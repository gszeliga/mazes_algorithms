(ns mazes.display.funplay
  (use [mazes.grid :only (make-grid)]
       [mazes.path :only (path-to)]
       [mazes.distances :only (distances-from)]
       [mazes.algorithms.events :only (poll! event-stream offer!)])
  (require [mazes.display.core :refer :all]
           [mazes.display.core-string :refer :all]))

(defn prn-grid
  [grid & {:keys [rendered] :or {rendered with-spaces}}]
  (-> grid (stringify rendered) (print)))

(defn string-it
  [rows cols & {:keys [using distance-at]}]
  (let [grid (using (make-grid rows cols))
        rendered-with (if (some? distance-at)
                        (with-distances (apply distances-from grid distance-at))
                        with-spaces)]
    (prn-grid grid :rendered rendered-with)))

(defn draw-it
  [rows cols & {:keys [using size with-path]
                :or {size 10}}]
  (let [grid (using (make-grid rows cols))
        path (when-not (nil? with-path) (apply path-to grid with-path))]
    (-> grid (draw :size size :with-path path))))

(defn animate-it
  [rows cols & {:keys [using size speed]
                :or {size 10 speed 50}}]
  (let [events (event-stream)]
    (-> (using (make-grid rows cols) #(offer! events %)) (animate! events :size size :speed speed))))
