(ns aoc-2021.day-9
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 9: Smoke Basin --- https://adventofcode.com/2021/day/9

(def task-input (u/slurp-resource "inputs/aoc_2021/day-9.txt"))

(def test-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")


(defn point-at
  [grid x y]
  (-> grid (nth y "") (nth x 1000) int))

(defn low-point?
  [grid value x y]
  (and (< value (point-at grid (dec x) y))
       (< value (point-at grid (inc x) y))
       (< value (point-at grid x (dec y)))
       (< value (point-at grid x (inc y)))))

(defn part-1
  [input]
  (let [grid (str/split-lines input)]
    (apply + (for [y (range (count grid))
                   x (range (count (first grid)))
                   :let [value (point-at grid x y)]
                   :when (low-point? grid value x y)]
               (- value 47)))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 15
  (part-1 task-input)                                       ; => 417
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (quick-bench (part-2 task-input))

  )
