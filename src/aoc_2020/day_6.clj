(ns aoc-2020.day-6
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 6: Custom Customs --- https://adventofcode.com/2020/day/6

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-6.txt"))


(defn count-any
  [input]
  (->> (string/split input #"\n\n")
       (map set)
       (map #(disj % \newline))
       (map count)
       (apply +)))



(comment
  ;; Part 1
  (count-any task-input)
  ;; Part 2

  )
