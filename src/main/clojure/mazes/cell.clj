(ns mazes.cell)

(defn ^:private link-fn [from-cell to-cell bidi]
  (dosync
   (alter (:links from-cell) assoc (to-id to-cell) to-cell))
  (when bidi (link-fn to-cell from-cell false)))

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
  (tunnel! [self cell])
  (tunnels [self]))

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
    (link-fn self cell bidi))
  (unlink [self cell]
    (unlink cell true))
  (unlink [self cell bidi]
    (dosync
     (alter (:links self) dissoc (to-id cell)))
    (when bidi (unlink cell self false)))
  (links [self] (keys (-> self :links deref))))

(defrecord WeavedCell [row column links dead tunnels tunnel-fn]
  Weaved
  (tunnel! [self cell]
    (swap! (self :tunnels) assoc (to-id cell) cell))
  (tunnels [self]
    (-> self :tunnels deref keys))
  
  Linkable
  (link [self cell bidi]
    (when (not (tunnel-fn self cell)) 
      (link-fn self cell bidi))))

(defn make-cell
      [& {:keys [row column links dead]
          :or {dead false links {}}}]
      (Cell. row column (ref links) dead))
