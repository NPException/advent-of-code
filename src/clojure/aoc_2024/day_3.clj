(ns aoc-2024.day-3
  (:require [aoc-utils :as u]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 3: Mull It Over ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-3.txt"))

(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def test-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")


(defn part-1
  [input]
  (->> input
       (re-seq #"mul\(\d{1,3},\d{1,3}\)")
       (map u/read-as-vector)
       (map second)
       (map #(apply * %))
       (apply +)))


(defn part-2
  [input]
  (->> input
       (re-seq #"do\(\)|don't\(\)|mul\(\d{1,3},\d{1,3}\)")
       (map u/read-as-vector)
       (reduce
         (fn [[result do? :as state] [op args]]
           (case op
             do [result true]
             don't [result false]
             mul (if do?
                   [(+ result (apply * args)) do?]
                   state)))
         [0 true])
       (first)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 161
  (part-1 task-input)                                       ; => 187825547
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-2)                                     ; => 48
  (part-2 task-input)                                       ; => 85508223
  (crit/quick-bench (part-2 task-input))

  )
