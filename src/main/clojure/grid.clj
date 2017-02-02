(ns grid)

(require 'cell)

(defn- neighbors-at [row column]
  [[(- 1 row) column]
   [(+ 1 row) column]
   [row (- 1 column)]
   [row (+ 1 column)]])

(defn make-grid [rows columns]
  (with-meta (into [] (map
                       (fn [r]
                         (into [] (map
                                   (fn [c] (cell/make-cell :row r :column c))
                                   (range columns))))
                       (range rows)))
    {:rows rows :columns columns}))

(defn n-rows [grid]
  (-> grid meta :rows))

(defn n-cols [grid]
  (-> grid meta :columns))

(defn cell-at [grid row col]
  (when (and (<= 0 row (n-rows grid))
             (<= 0 col (n-cols grid)))
    (get-in grid [row col])))

(defn rand-cell-at [grid]
  (let [row (long (rand (n-rows grid)))
        col (long (rand (n-cols grid)))]
    (cell-at grid row col)))

(defn neighbors-from [grid cell]
  (let [row (:row cell)
        col (:column cell)]
    (map #(apply cell-at grid %) (neighbors-at row col))))

(defn rows-from [grid]
  grid)

(defn cols-from [grid]
  (apply map vector grid))

(defn cells-from [grid]
  ;We could use just:
  ; (flatten grid)
  ;or 
  ; (into (empty grid) (mapcat identity grid)))

  (reduce #(if (seq? %2)
             (concat %1 (cells-from %2))
             (concat %1 (seq %2)))
          (empty grid) grid))
