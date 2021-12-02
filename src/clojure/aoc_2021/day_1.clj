(ns aoc-2021.day-1
  (:use [criterium.core])
  (:require [aoc-utils :as u]
            [clojure.edn :as edn]))

;; --- Day 1: Sonar Sweep --- https://adventofcode.com/2021/day/1

(def task-input (edn/read-string (str "[" (u/slurp-resource "inputs/aoc_2021/day-1.txt") "]")))

(def test-input (edn/read-string "[ 199\n200\n208\n210\n200\n207\n240\n269\n260\n263 ]"))


(defn count-increasing-steps
  [input transform]
  (transduce
    (comp transform
          (u/partition-xf 2 1)
          (map (fn [[a b]]
                 (if (< a b) 1 0))))
    +
    input))

(defn part-1
  [input]
  (count-increasing-steps input identity))


(defn part-2
  [input]
  (count-increasing-steps input (comp (u/partition-xf 3 1)
                                      (map #(apply + %)))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 7
  (part-1 task-input)                                       ; => 1832

  ;; Part 2
  (part-2 test-input)                                       ; => 5
  (part-2 task-input)                                       ; => 1858

  )
