(ns mazes.mask
  (require [clojure.java.io :as io]))

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
    (.set (. self bits) (+ (* row (. self cols)) col) v)
    self))

(defn make-mask [rows columns]
  (let [size (* rows columns)
        bits (java.util.BitSet. size)
        _ (.flip bits 0 size)]
    (->Mask rows columns bits)))

(defn from-txt [file]
  (with-open [rdr (io/reader file)]
    (let [bits-idx (into () ;it sucks that it can't be lazy but I need [rows,columns]
                         (for [[line-idx,line] (map-indexed #(vector %1 %2) (reverse (line-seq rdr)))
                               cols-idx (map-indexed #(vector [line-idx %1] (== (int \.) (int %2))) line)]
                           cols-idx))
          ;little 'trick' here: since a list guarantees 'head' to be the latest inserted element, it is expected to
          ;contain the upper-right most cell coordinate 'matching' the number of rows & cols in the grid
          [rows,cols] (->> bits-idx first first (map inc))]

      (reduce (fn [mask [[row col] bit]]
                (define mask row col bit))
              (make-mask rows cols) bits-idx))))
