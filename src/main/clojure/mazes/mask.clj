(ns mazes.mask)

(defprotocol GridMask
  (on? [self row col])
  (off? [self row col])
  (define [self row col v])
  (size [self]))

(deftype Mask [rows cols bits]
  GridMask
  (on? [self row col]
    (.get (. self bits) (+ (* row (. self cols)) col)))
  (off? [self row col]
    (not (on? self row col)))
  (size [self]
    (.cardinality (. self bits)))
  (define [self row col v]
    (.set (. self bits) (+ (* row (. self cols)) col) v)))

(defn make-mask [rows columns]
  (let [size (* rows columns)
        bits (java.util.BitSet. size)
        _ (.flip bits 0 size)]
    (->Mask rows columns bits)))
