(ns aoc-2022.day-3
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 3: Rucksack Reorganization ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-3.txt"))

(def test-input "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv (fn [line]
               (let [len  (count line)
                     half (/ len 2)]
                 [(set (subs line 0 half))
                  (set (subs line half))])))))


(defn priority
  [char]
  (let [c (int char)]
    (if (>= c 97)
      (- c 96)
      (- c 38))))


(defn part-1
  [input]
  (->> (parse-input input)
       (mapcat #(apply set/intersection %))
       (map priority)
       (apply +)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 157
  (part-1 task-input)                                       ; => 7674
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
