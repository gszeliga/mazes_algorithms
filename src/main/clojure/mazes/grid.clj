(ns mazes.grid
  (require [mazes.cell :as cell :refer :all])
  (import [mazes.cell.Cell]))

(defn- neighbors-at [row column]
  {:north [(inc row) column]
   :south [(dec row) column]
   :west  [row (dec column)]
   :east  [row (inc column)]})

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

(defn n-cells [grid]
  (* (n-cols grid) (n-rows grid)))

(defn cell-at
  ([grid cell]
   (cell-at grid (:row cell) (:column cell)))
  ([grid row col]
   (when (and (<= 0 row (n-rows grid))
              (<= 0 col (n-cols grid)))
     (get-in grid [row col]))))

(defn rand-cell-at [grid]
  (let [row (long (rand (n-rows grid)))
        col (long (rand (n-cols grid)))]
    (cell-at grid row col)))

(defn neighbors-from
  ([cell grid]
   (neighbors-from (:row cell) (:column cell) grid))
  ([row col grid]
   (reduce-kv
    (fn [m k coord] (assoc m k (apply cell-at grid coord)))
    {}
    (neighbors-at row col))))

(defn neighbors-available
  ([cell grid]
   (neighbors-available (:row cell) (:column cell) grid))
  ([row col grid]
   (->> (neighbors-from row col grid) vals (filter some?))))

(defn neighbors-not-linked
  ([cell grid]
   (neighbors-not-linked (:row cell) (:column cell) grid))
  ([row col grid]
   (->> (neighbors-available row col grid) (filter #(empty? (links %))))))

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
