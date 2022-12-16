(ns aoc-2016.day-3
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 3: Squares With Three Sides ---

(def triangles (->> (u/slurp-resource "inputs/aoc_2016/day-3.txt")
                    (str/split-lines)
                    (mapv u/read-as-vector)))


(defn valid-triangle?
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))


(defn part-1
  []
  (u/count-matching valid-triangle? triangles))


(defn part-2
  []
  (->> (u/transpose triangles)
       (apply concat)
       (partition 3)
       (u/count-matching valid-triangle?)))


(comment
  ;; Part 1
  (part-1)                                       ; => 983
  (crit/quick-bench (part-1))

  ;; Part 2
  (part-2)                                       ; => 1836
  (crit/quick-bench (part-2))

  )
