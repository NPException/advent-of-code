(ns aoc-2020.day-6
  (:require [clojure.string :as string]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 6: Custom Customs --- https://adventofcode.com/2020/day/6

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-6.txt"))

(def groups
  (string/split task-input #"\n\n"))


(defn count-via
  [set-op]
  #(->> (string/split-lines %)
        (map set)
        (reduce set-op)
        count))


(comment
  ;; Part 1
  (apply + (map (count-via set/union) groups))
  ;; Part 2
  (apply + (map (count-via set/intersection) groups))
  )
