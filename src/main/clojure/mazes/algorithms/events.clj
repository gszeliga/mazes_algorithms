(ns mazes.algorithms.events
  (require [mazes.cell :refer :all]))

(defn event-stream [] (java.util.concurrent.LinkedBlockingDeque.))

(defn offer!
  "adds x to the back of queue q"
  [q x] (.offer q x) q)

(defn take!
  "takes from the front of queue q.  if q is empty, block until something is offer!ed into it"
  [q] (.take q))

(defn poll!
  ([q] (poll! #{} q))
  ([accepted-types q] (poll! 1 accepted-types q))
  ([n-of-events accepted-types q]

   (defn do-poll [accepts? collected]
     (if-not (= n-of-events (count collected))
       (if-let [{event-type :type :as event} (.poll q)]
         (recur accepts? (if (accepts? event-type)
                           (conj collected event)
                           collected))
         collected)
       collected))

   (defn just [events]
     (fn [e] (or (empty? events) (contains? events e))))

   (do-poll (just (set (if-not (seq? accepted-types)
                         [accepted-types]
                         accepted-types))) [])))

(defn just-types
  [& only-types]
  (defn keeping [types]
    (fn [e] (or (empty? types) (contains? types (:type e)))))
  (filter (keeping (set only-types))))

(defn wall-down-emiter [f]
  (fn [from to]
    (f {:type :wall-down :values [(to-id from) (to-id to)]})))

(defn visiting-cell-id-emiter [f]
  (fn [cell-id]
    (f {:type :visiting :values cell-id})))

(defn visiting-cell-emiter [f]
  (fn [cell]
    (f {:type :visiting :values (to-id cell)})))
