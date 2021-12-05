(ns aoc-2021.day-5
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]
            [clojure.edn :as edn]))

;; --- Day 5: Hydrothermal Venture --- https://adventofcode.com/2021/day/5

(def task-input (u/slurp-resource "inputs/aoc_2021/day-5.txt"))

(def test-input "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2")

(defn parse-lines
  [input]
  (->> (str/split-lines input)
       (mapv #(edn/read-string (str "[" (str/replace % "->" "") "]")))))


(defn straight?
  [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))


(defn between
  [a b]
  (if (<= a b)
    (range a (inc b))
    (range a (dec b) -1)))


(defn points-on-line
  [[x1 y1 x2 y2 :as line]]
  (let [xs (between x1 x2)
        ys (between y1 y2)]
    (if (straight? line)
      (for [x xs, y ys] [x y])
      (mapv vector xs ys))))


(defn count-overlapping-points
  [input valid-line?]
  (->> (parse-lines input)
       (filterv valid-line?)
       (mapcat points-on-line)
       (frequencies)
       (u/count-matching #(>= (val %) 2))))


(defn part-1
  [input]
  (count-overlapping-points input straight?))

(defn part-2
  [input]
  (count-overlapping-points input (constantly true)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 5
  (part-1 task-input)                                       ; => 6564
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 12
  (part-2 task-input)                                       ; => 19172
  (quick-bench (part-2 task-input))

  )
