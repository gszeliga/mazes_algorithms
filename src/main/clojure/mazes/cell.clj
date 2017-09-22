(ns mazes.cell)

(defrecord Cell [row column links dead])

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

(extend-protocol Linkable
  Cell
  (to-id [self]
    [(:row self) (:column self)])

  (dead? [self]
    (:dead self))

  (linked? [self cell]
    (if cell
      (contains? (-> self :links deref) (to-id cell))
      false))

  (link
    ([self cell] (link self cell true))
    ([self cell bidi]
     (dosync
      (alter (:links self) assoc (to-id cell) cell))
     (when bidi (link cell self false))))

  (unlink
    ([self cell] (unlink cell true))
    ([self cell bidi]
     (dosync
      (alter (:links self) dissoc (to-id cell)))
     (when bidi (unlink cell self false))))

  (links [self] (keys (-> self :links deref))))

(defn make-cell
  [& {:keys [row column links dead] :or {dead false links {}}}]
  (Cell. row column (ref links) dead))

; (defn make-cell
;    ([row column] (make-cell row column {}))
;    ([row column links] (Cell. row column links)))
