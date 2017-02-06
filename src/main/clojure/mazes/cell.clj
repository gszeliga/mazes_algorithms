(ns mazes.cell)

(defrecord Cell [row column links])

(defprotocol Linkable
  (linked? [self cell])
  (link [self cell bidi])
  (unlink [self cell bidi])
  (links [self]))

(extend-protocol Linkable
  Cell
  (linked? [self cell]
    (contains? (-> self :links deref) cell))

  (link [self cell bidi]
    (dosync
     (alter (:links self) assoc cell true))
    (when bidi (link cell self false)))

  (unlink [self cell bidi]
    (dosync
     (alter (:links self) dissoc cell))
    (when bidi (unlink cell self false)))

  (links [self] (keys (-> self :links deref))))

(defn make-cell
  [& {:keys [row column links] :or {links {}}}]
  (Cell. row column (ref links)))

; (defn make-cell
;    ([row column] (make-cell row column {}))
;    ([row column links] (Cell. row column links)))
