(ns mazes.algorithms.events
  (require [mazes.cell :refer :all]))

(defn event-stream [] (java.util.concurrent.LinkedBlockingDeque.))

(defn offer!
  "adds x to the back of queue q"
  [q x] (.offer q x) q)

(defn take!
  "takes from the front of queue q.  if q is empty, block until something is offer!ed into it"
  [q] (.take q))

(defn tear-down-wall-emiter [f]
  (fn [from to]
    (f {:tear-down-wall #{(to-id from) (to-id to)}})))

(defn visiting-cell-emiter [f]
  (fn [cell]
    (f {:visiting (to-id cell)})))
