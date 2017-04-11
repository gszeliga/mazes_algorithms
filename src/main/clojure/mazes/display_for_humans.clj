(ns mazes.display-for-humans
  (use [mazes.grid :only (make-grid)]
       [mazes.algorithms.events :only (poll! event-stream offer!)])
  (require [mazes.display :refer :all]))

(defn prn-grid
  [grid & {:keys [rendered] :or {rendered with-spaces}}]
  (-> grid (stringify rendered) (print)))

(defn string-it
  [rows cols & {:keys [using]}]
  (-> (using (make-grid rows cols)) (prn-grid)))

(defn draw-it
  [rows cols & {:keys [using size]
                :or {size 10}}]
  (-> (using (make-grid rows cols)) (draw :size size)))

(defn animate-it
  [rows cols & {:keys [using size speed]
                :or {size 10 speed 50}}]
  (let [events (event-stream)]
    (-> (using (make-grid rows cols) #(offer! events %)) (animate! events :size size :speed speed))))
