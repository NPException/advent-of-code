(ns aoc-2022.day-6
  (:require [aoc-utils :as u]
            [criterium.core :as crit]))

;; --- Day 6: Tuning Trouble ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-6.txt"))

(def test-input "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")

(defn find-marker
  [input n]
  (->> (partition n 1 input)
       (u/index-of #(apply distinct? %))
       (+ n)))

(defn part-1
  [input]
  (find-marker input 4))

(defn part-2
  [input]
  (find-marker input 14))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 10
  (part-1 task-input)                                       ; => 1779
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 29
  (part-2 task-input)                                       ; => 2635
  (crit/quick-bench (part-2 task-input))

  )
