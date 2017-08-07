(ns mazes.grid
  (require [mazes.cell :refer :all])
  (require [mazes.mask :refer :all])
  (import [mazes.cell.Cell]))

(defn ^:private neighbors-at [row column]
  {:north [(inc row) column]
   :south [(dec row) column]
   :west  [row (dec column)]
   :east  [row (inc column)]})

(defn make-grid
  ([rows columns]
   (make-grid rows columns (make-mask rows columns)))
  ([rows columns mask]
   (with-meta (into [] (map
                        (fn [r]
                          (into []
                                (map #(make-cell :row r :column % :dead (off? mask r %)))
                                (range columns)))
                        (range rows)))
     {:rows rows :columns columns :mask mask})))

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
    (fn [m k coord]
      (let [cell (apply cell-at grid coord)
            is-dead (if (some? cell) (dead? cell) true)]
        (assoc m k (when-not is-dead cell))))
    {}
    (neighbors-at row col))))

(def ^:private cell-xforms
  {:present (filter some?)
   :linked (comp (filter some?) (map (fn [v] [v (->> v links not-empty)])) (filter second) (map first))
   :not-linked (comp (filter some?) (map (fn [v] [v (->> v links empty?)])) (filter second) (map first))
   :all (map identity)})

(defn neighbors
  ([cell grid]
   (neighbors (:row cell) (:column cell) grid :all))
  ([cell grid state]
   (neighbors (:row cell) (:column cell) grid state))
  ([row col grid state]
   (sequence (state cell-xforms) (vals (neighbors-from row col grid)))))

(defn rows-from [grid]
  grid)

(defn cols-from [grid]
  (apply map vector grid))

(defn cells-from
  ([grid]
   (cells-from grid :all))
  ([grid state]
   (sequence (state cell-xforms) (mapcat identity grid))))
