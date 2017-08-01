(ns mazes.mask)

(defprotocol GridMask
  (on? [self row col]))

(deftype Mask [rows cols bits]
  GridMask
  (on? [self row col] (.get (. self bits) (+ (* row (. self cols)) col))))

(defn make-mask [rows cols]
  (let [size (* rows cols)
        bits (java.util.BitSet. size)
        _ (.flip bits 0 size)]
    (->Mask rows cols bits)))
