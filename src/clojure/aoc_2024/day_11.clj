(ns aoc-2024.day-11
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [criterium.core :as crit])
  (:import (java.util HashMap)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 11: Plutonian Pebbles ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-11.txt"))

(def test-input "125 17")



; If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
(defn change-zero
  [^long stone]
  (when (zero? stone)
    [1]))

; If the stone is engraved with a number that has an even number of digits, it is replaced by two stones.
; The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone.
; (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
(defn change-even-digits
  [^long stone]
  (let [n (u/num-digits stone)]
    (when (even? n)
      (let [div-by (long (math/pow 10 (quot n 2)))]
        [(quot stone div-by)
         (rem stone div-by)]))))

;If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
(defn change-multiply
  [^long stone]
  [(* stone 2024)])


(defn change-stone
  [stone]
  (or (change-zero stone)
      (change-even-digits stone)
      (change-multiply stone)))


(declare count-sub-stone)

(defn count-stones
  [stones ^long iterations ^HashMap cache]
  (->> stones
       (mapv #(count-sub-stone % iterations cache))
       (apply +)))

(defn count-sub-stone
  [stone ^long iterations ^HashMap cache]
  (if (= 1 iterations)
    (count (change-stone stone))
    (if-let [cached (.get cache [stone iterations])]
      cached
      (let [computed (count-stones (change-stone stone) (dec iterations) cache)]
        (.put cache [stone iterations] computed)
        computed))))


(defn part-1
  [input]
  (count-stones (u/read-as-vector input) 25 (HashMap.)))


(defn part-2
  [input]
  (count-stones (u/read-as-vector input) 75 (HashMap.)))



(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 55312
  (part-1 task-input)                                       ; => 211306
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 task-input)                                       ; => 250783680217283
  (crit/quick-bench (part-2 task-input))

  )
