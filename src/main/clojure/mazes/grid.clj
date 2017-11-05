(ns mazes.grid
  (require [mazes.cell :refer :all]
           [mazes.mask :refer :all])
  (import [mazes.cell.Cell]
          [mazes.mask.Mask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                       Cell XFORMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private cell-xforms
  (letfn [(cell-in [v]
            (if (vector? v) (second v) v))]

    {:present (filter (comp some? cell-in))

     :linked (comp
              (filter (comp some? cell-in))
              (filter #(->> % cell-in links empty? not)))

     :not-linked (comp
                  (filter (comp some? cell-in))
                  (filter #(->> % cell-in links empty?)))

     :dead-ends (comp
                 (filter (comp some? cell-in))
                 (filter #(->> % cell-in links count (= 1))))

     :all (map identity)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Grid metadata & accesors 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn n-rows [grid]
  (-> grid meta :rows))

(defn n-cols [grid]
  (-> grid meta :columns))

(defn n-cells [grid]
  (* (n-cols grid) (n-rows grid)))

(defn rows-from [grid]
  grid)

(defn cols-from [grid]
  (apply map vector grid))

(defn cells-from
  ([grid]
   (cells-from grid :all))
  ([grid state]
   (sequence (comp (remove :dead) (state cell-xforms)) 
             (mapcat identity grid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Cell accesors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti cell-at 
  (fn [grid & _] (-> grid meta :type)))

(defmethod cell-at :polar
  ([grid cell]
   (cell-at grid (:row cell) (:column cell)))
  ([grid row col]
   (let [adj-col (mod col (-> grid (get row) count))]
     (get-in grid [row adj-col]))))

(defmethod cell-at :default
  ([grid cell]
   (cell-at grid (:row cell) (:column cell)))
  ([grid row col]
   (get-in grid [row col])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                    Grid definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti make-grid (fn [t & args] t))

(defmethod make-grid :polar 
  [t rows columns]
  (let [row-height (/ 1.0 rows)]
    (with-meta (reduce (fn [partial-grid row]
                         (let [radius               (/ (float row) rows)
                               circumference        (* 2 (Math/PI) radius)
                               previous-count       (count (get partial-grid (dec row)))
                               estimated-cell-width (/ circumference previous-count)
                               ratio                (Math/round (/ estimated-cell-width row-height))
                               n-cells              (* previous-count ratio)]

                           (conj partial-grid (into [] (map #(make-cell :row row :column % :dead false)
                                                            (range n-cells))))))
                       [[(make-cell :row 0 :column 0 :dead false)]]
                       (range 1 rows))
      {:rows rows :columns columns :type t})))

(defmethod make-grid :default
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Random resolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti rand-cell-at #(-> % meta :type))

(defmethod rand-cell-at :polar
  [grid]
  (let [row (long (rand (n-rows grid)))
        col (long (rand (-> grid (get row) count)))]
    (cell-at grid row col)))

(defmethod rand-cell-at :default
  [grid]
  (let [row (long (rand (n-rows grid)))
        col (long (rand (n-cols grid)))]
    (cell-at grid row col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                       Neighbors resolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ^:private neighbors-fn 
  [row col grid coords-fn]
  (reduce-kv
   (fn [m k coord]
     (let [cell    (when coord (apply cell-at grid coord))
           is-dead (if (some? cell) (dead? cell) true)]
       (assoc m k (when-not is-dead cell))))
   {}
   (coords-fn row col)))

(defmulti ^:private neighbors-from
  (fn [row col grid] (-> grid meta :type)))

(defmethod neighbors-from :standard
  [row col grid]
  (letfn [(coords-fn [row column]
            {:north [(inc row) column]
             :south [(dec row) column]
             :west  [row (dec column)]
             :east  [row (inc column)]})]
    (neighbors-fn row col grid coords-fn)))

(defmethod neighbors-from :sigma
  [row col grid]
  (letfn [(coords-fn [row col]
            (let [north-diagonal (if (even? col) (dec row) row)
                  south-diagonal (if (even? col) row (inc row))]
              {:northwest [north-diagonal (dec col)]
               :north     [(dec row) col]
               :northeast [north-diagonal (inc col)]
               :southwest [south-diagonal (dec col)]
               :south     [(inc row) col]
               :southeast [south-diagonal (inc col)]}))]
    (neighbors-fn row col grid coords-fn)))

(defmethod neighbors-from :polar
  [row col grid]
  (letfn [(ratio-fn [row grid]
            (when (and (-> row zero? not)
                       (< row (n-rows grid)))
              (let [c-row-length (-> grid (get row) count)
                    p-row-length (-> grid (get (dec row)) count)]
                (/ c-row-length p-row-length))))

          (coords-fn [row column]
            (if (zero? row)
              {:cw      nil
               :ccw     nil
               :outward [1 (* column (ratio-fn 1 grid))]
               :inward  nil}
              (let [o-ratio (ratio-fn (inc row) grid)
                    c-ratio (ratio-fn row grid)]
                {:cw      [row (inc column)]
                 :ccw     [row (dec column)]
                 :outward (when o-ratio [(inc row) (* column o-ratio)])
                 :inward  [(dec row) (/ column c-ratio)]})))]

    (neighbors-fn row col grid coords-fn)))

(defmethod neighbors-from :triangle
  [row col grid]
  (letfn [(coords-fn [row col]
            (let [upright? (even? (+ row col))]
              {
               :north (when (not upright?) [(dec row) col])
               :south (when upright? [(inc row) col])
               :west  [row (dec col)]
               :east  [row (inc col)]}))]
    (neighbors-fn row col grid coords-fn)))

(defn neighbors
  ([cell grid]
   (neighbors (:row cell) (:column cell) grid :all))
  ([cell grid state]
   (neighbors (:row cell) (:column cell) grid state))
  ([row col grid state]
   (into {}
         (comp (remove :dead)
               (state cell-xforms))
         (neighbors-from row col grid))))
