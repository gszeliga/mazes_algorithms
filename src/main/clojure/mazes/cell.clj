(ns mazes.cell)

(defprotocol Linkable
  (to-id [self])
  (linked? [self cell])
  (dead? [self])
  (link
    [self cell]
    [self cell bidi])
  (unlink
    [self cell]
    [self cell bidi])
  (links [self]))

(defprotocol Weaved
  (over?  [self])
  (under? [self])
  (vert-pos [self pos])
  (horizontal-passage? [self neighbors])
  (vertical-passage? [self neighbors]))

(defrecord Cell [row column links dead]
  Linkable

  (to-id [self]
    [(:row self) (:column self)])

  (dead? [self] (:dead self))

  (linked? [self cell]
    (if cell
      (contains? (-> self :links deref)
                 (to-id cell))
      false))

  (link [self cell]
    (link self cell true))

  (link [self cell bidi]
    (dosync
     (alter (:links self) assoc (to-id cell) cell))
    (when bidi (link cell self false)))

  (unlink [self cell]
    (unlink cell true))

  (unlink [self cell bidi]
    (dosync
     (alter (:links self) dissoc (to-id cell)))
    (when bidi (unlink cell self false)))

  (links [self] (keys (-> self :links deref))))

(defrecord WeavedCell [row column links dead vert-pos]
  Weaved
  (over? [self]
    (-> self :vert-pos deref (= :over)))

  (under? [self]
    (-> self :vert-pos deref (= :under))))

(defn make-cell
      [& {:keys [row column links dead]
          :or {dead false links {}}}]
      (Cell. row column (ref links) dead))
