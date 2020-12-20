(ns aoc-2015.day-10
  (:require [aoc-utils :as u]))

;; --- Day 10: Elves Look, Elves Say --- https://adventofcode.com/2015/day/10

(def task-input (u/slurp-resource "inputs/aoc_2015/day-10.txt"))


(defn look-and-say
  ([col]
   (->> (partition-by identity col)
        (mapcat (juxt count first))
        (apply str)))
  ([col times]
   (nth (iterate look-and-say col) times)))


(defn part-1
  [input]
  (count (look-and-say input 40)))


(defn part-2
  [input]
  (count (look-and-say input 50)))


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 252594

  ;; Part 2
  (part-2 task-input)                                       ; => 3579328

  )
