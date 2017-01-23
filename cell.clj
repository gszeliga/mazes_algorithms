(defrecord Cell [row column links])

(defprotocol Linkable
  (linked? [cell])
  (link [cell, bidi])
  (unlink [cell, bidi])
  (links [cell]))

(defn make-cell
  [& {:keys [row column links] :or {links {}}}]
  (Cell. row column links))

; (defn make-cell
;    ([row column] (make-cell row column {}))
;    ([row column links] (Cell. row column links)))
