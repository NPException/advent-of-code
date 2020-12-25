(ns aoc-2020.day-25
  (:require [aoc-utils :as u]))

;; --- Day 25: Combo Breaker --- https://adventofcode.com/2020/day/25

(def task-input (u/slurp-resource "inputs/aoc_2020/day-25.txt"))

(def test-input "5764801\n17807724")


(defn hash-round
  [subject-number value]
  (mod (* subject-number value)
       20201227))


(defn crack
  [input]
  (let [[key-a key-b] (read-string (str "[" input "]"))]
    (loop [value (hash-round 7 1)
           encryption-key (hash-round key-b 1)]
      (if (= key-a value)
        encryption-key
        (recur (hash-round 7 value)
               (hash-round key-b encryption-key))))))


(comment
  ;; Part 1
  (crack test-input)                                       ; => 14897079
  (crack task-input)                                       ; => 19774660

  )
