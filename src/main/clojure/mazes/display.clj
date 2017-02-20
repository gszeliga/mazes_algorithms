(ns mazes.display
  (use [mazes.grid :only (rows-from
                          neighbors-from
                          n-cols
                          n-rows
                          cells-from)])

  (require [mazes.cell :refer :all] :reload
           [quil.core :as q :include-macros true])
  (:gen-class))

(defn stringify [grid]

  (defn display-row-cell [row-line cell]
    (let [[row-line-top row-line-bottom] row-line
          neighbors (neighbors-from cell grid)
          east-cell (:east neighbors)
          south-cell (:south neighbors)]

      [(str row-line-top "   " (if (and (some? east-cell)
                                        (linked? cell east-cell))
                                 " "
                                 "|"))

       (str row-line-bottom (if (and (some? south-cell)
                                     (linked? cell south-cell))
                              "   "
                              "---") "+")]))

  (defn display-grid-row [grid-as-string row]
    (str (apply str grid-as-string
                (interpose \newline (reduce display-row-cell ["|" "+"] row)))
         \newline))

  (reduce display-grid-row
          (str "+" (apply str (repeat (n-cols grid) "---+")) \newline)
          (reverse (rows-from grid))))

(defn draw [grid]

  (def pixels-per-cell 40)

  (defn setup []
    (q/background 255))

  ;TODO Need to introduce the idea of "wall" which means that we must plot 'lines' instead of 'rectangles'
  (defn plot-cell [cell]
    (q/rect (* pixels-per-cell (:column cell))
            (* pixels-per-cell (:row cell))
            pixels-per-cell cell-size))

  (defn do-draw []
    (doseq [cell (cells-from grid)]
      (plot-cell cell)))

  (q/defsketch sample-maze
    :size [(* (n-cols grid) pixels-per-cell)
           (* (n-rows grid) pixels-per-cell)]
    :setup setup
    :draw do-draw))

;TODO The idea is to end up using functinal mode: https://github.com/quil/quil/wiki/Functional-mode-%28fun-mode%29
(defn draw-animated [grid]

  (def pixels-per-cell 40)

  (defn setup []
      ; draw will be called 60 times per second
    (q/frame-rate 10)
      ; set background to white colour only in the setup
      ; otherwise each invocation of 'draw' would clear sketch completely
    (q/background 255))

  (defn do-draw []
    (doseq [cell (take 1 (drop (dec (q/frame-count)) (cells-from grid)))]
      (q/rect (* pixels-per-cell (:column cell))
              (* pixels-per-cell (:row cell))
              pixels-per-cell cell-size)))

  (q/defsketch sample-maze
    :size [(* (n-cols grid) pixels-per-cell)
           (* (n-rows grid) pixels-per-cell)]
    :setup setup
    :draw do-draw))
