(ns aoc-2022.day-1
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

;; --- Day 1: Calorie Counting ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-1.txt"))

(def test-input "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")


(defn read-summed-calories
  [input]
  (->>(str/split input #"\n\n")
      (mapv u/read-as-vector)
      (mapv #(reduce + %))))


(defn part-1
  [input]
  (->> (read-summed-calories input)
       (apply max)))


(defn part-2
  [input]
  (->> (read-summed-calories input)
       (sort >)
       (take 3)
       (apply +)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 24000
  (part-1 task-input)                                       ; => 69206
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 45000
  (part-2 task-input)                                       ; => 197400
  (crit/quick-bench (part-2 task-input))

  )
