(ns aoc-2020.day-6
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 6: Custom Customs --- https://adventofcode.com/2020/day/6

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-6.txt"))

(def groups
  (string/split task-input #"\n\n"))


;; Part 1 counter
(defn count-any
  [group]
  (-> (set group)
      (disj \newline)
      count))



(comment
  ;; Part 1
  (apply + (map count-any groups))
  ;; Part 2

  )
