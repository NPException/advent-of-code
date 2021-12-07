(ns aoc-2021.day-7
  (:use [criterium.core])
  (:require [aoc-utils :as u]))

;; --- Day 7: The Treachery of Whales --- https://adventofcode.com/2021/day/7

(def task-input (read-string (str "[" (u/slurp-resource "inputs/aoc_2021/day-7.txt") "]")))

(def test-input (read-string "[16,1,2,0,4,2,7,1,2,14]"))

(defn realign-crabs
  [input target calc-fuel]
  (transduce (comp (map #(- target %))
                   (map #(Math/abs ^long %))
                   (map calc-fuel))
             + input))

(defn part-1
  [input]
  (let [median (nth (sort input) (quot (count input) 2))]
    (realign-crabs input median identity)))

(defn part-2
  [input]
  (let [calc (fn [target] (realign-crabs input target #(quot (* % (inc %)) 2)))]
    (let [mean (quot (apply + input) (count input))]
      (min (calc mean) (calc (inc mean))))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 37
  (part-1 task-input)                                       ; => 356958
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 168
  (part-2 task-input)                                       ; => 105461913
  (quick-bench (part-2 task-input))

  )
