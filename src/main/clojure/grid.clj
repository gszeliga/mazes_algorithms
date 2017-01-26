(ns grid)

(require 'cell)

(defn make-grid [rows columns]
  (into [] (map
            (fn [r]
              (into [] (map
                        (fn [c] (cell/make-cell :row r :column c))
                        (range columns))))
            (range rows))))

(defn neighbors-of [grid cell])
(defn rows-from [grid])
(defn cells-from [grid])
