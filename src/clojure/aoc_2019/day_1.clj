(ns aoc-2019.day-1
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 1: The Tyranny of the Rocket Equation ---

(def task-input (u/slurp-resource "inputs/aoc_2019/day-1.txt"))

(def test-input "12\n14\n1969\n100756")


(defn fuel-requirement
  ^long
  [^long mass]
  (- (quot mass 3) 2))


(defn part-1
  [input]
  (->> (u/read-as-vector input)
       (map fuel-requirement)
       (apply +)))


(defn corrected-fuel-requirement
  [mass]
  (loop [total 0
         fuel (fuel-requirement mass)]
    (if (<= fuel 0)
      total
      (recur
        (+ total fuel)
        (fuel-requirement fuel)))))


(defn part-2
  [input]
  (->> (u/read-as-vector input)
       (map corrected-fuel-requirement)
       (apply +)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 34241
  (part-1 task-input)                                       ; => 3401852
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 51316
  (part-2 task-input)                                       ; => 5099916
  (crit/quick-bench (part-2 task-input))

  )
