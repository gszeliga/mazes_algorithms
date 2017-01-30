(ns grid)

(require 'cell)

(defn make-grid [rows columns]
  (with-meta (into [] (map
                       (fn [r]
                         (into [] (map
                                   (fn [c] (cell/make-cell :row r :column c))
                                   (range columns))))
                       (range rows)))
    {:rows rows :columns columns}))

(defn cell-at [grid row col]
  (when (and (<= 0 row (-> grid meta :rows))
             (<= 0 col (-> grid meta :columns)))
    (get-in grid [row col])))

(defn neighbors-from [grid cell]
  (let [row (:row cell)
        col (:column cell)]
    (map #(apply cell-at grid %) (neighbors-at row col))))

(defn- neighbors-at [row column]
  [[(- 1 row) column]
   [(+ 1 row) column]
   [row (- 1 column)]
   [row (+ 1 column)]])

(defn rows-from [grid])
(defn cells-from [grid])
