(ns mazes.grid
  (require [mazes.cell :refer :all])
  (require [mazes.mask :refer :all])
  (import [mazes.cell.Cell]))

(defn ^:private neighbors-at [row column]
  {:north [(inc row) column]
   :south [(dec row) column]
   :west  [row (dec column)]
   :east  [row (inc column)]})

(defmulti make-grid (fn [t & args] t))

(defmethod make-grid :polar [t rows columns]

  (let [row-height (/ 1.0 rows)]

    (with-meta (reduce (fn [partial-grid row]
                         (let [radius (/ (float row) rows)
                               circumference (* 2 (Math/PI) radius)
                               previous-count (count (get partial-grid (dec row)))
                               estimated-cell-width (/ circumference previous-count)
                               ratio (Math/round (/ estimated-cell-width row-height))
                               n-cells (* previous-count ratio)]

                           (conj partial-grid (into [] (map #(make-cell :row row :column % :dead false)
                                                            (range n-cells))))))

                       [[(make-cell :row 0 :column 0 :dead false)]]
                       (range 1 rows))
      {:rows rows :columns columns :type t})))

(defmethod make-grid :standard
  ([t rows columns]
   (make-grid t rows columns (make-mask rows columns)))

  ([t rows columns mask]
   (with-meta (into [] (map
                        (fn [r]
                          (into []
                                (map #(make-cell :row r :column % :dead (off? mask r %)))
                                (range columns)))
                        (range rows)))
     {:rows rows :columns columns :mask mask :type t})))

(defmethod make-grid :default
  ([rows columns] (make-grid :standard rows columns))
  ([rows columns mask] (make-grid :standard rows columns mask)))

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
   :linked (comp (filter some?) (filter #(->> % links empty? not)))
   :not-linked (comp (filter some?) (filter #(->> % links empty?)))
   :all (map identity)})

;TODO necesito version de neighbors que me den key/values
;TODO hacer neighbors-from privado
;TODO hacer neighbors multimethod

(defn neighbors
  ([cell grid]
   (neighbors (:row cell) (:column cell) grid :all))
  ([cell grid state]
   (neighbors (:row cell) (:column cell) grid state))
  ([row col grid state]
   (sequence (comp (remove :dead) (state cell-xforms)) (vals (neighbors-from row col grid)))))

(defn rows-from [grid]
  grid)

(defn cols-from [grid]
  (apply map vector grid))

(defn cells-from
  ([grid]
   (cells-from grid :all))
  ([grid state]
   (sequence (comp (remove :dead) (state cell-xforms))  (mapcat identity grid))))
