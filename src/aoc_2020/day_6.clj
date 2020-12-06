(ns aoc-2020.day-6
  (:require [clojure.string :as string]
            [aoc-utils :as u]
            [clojure.set :as set]))

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


;; Part 2 counter
(defn count-every
  [group]
  (->> (string/split-lines group)
       (map set)
       (reduce set/intersection)
       count))



(comment
  ;; Part 1
  (apply + (map count-any groups))
  ;; Part 2
  (apply + (map count-every groups))
  )
