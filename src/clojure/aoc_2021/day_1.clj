(ns aoc-2021.day-1
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 1: Sonar Sweep --- https://adventofcode.com/2021/day/1

(def task-input (u/slurp-resource "inputs/aoc_2021/day-1.txt"))

(def test-input "199\n200\n208\n210\n200\n207\n240\n269\n260\n263")


(defn do-the-thing
  [input transform]
  (->> (str/split-lines input)
       (map parse-long)
       transform
       (partition 2 1)
       (filter #(apply < %))
       count))

(defn part-1
  [input]
  (do-the-thing input identity))


(defn sum-windows
  [nums]
  (->> (partition 3 1 nums)
       (map #(apply + %))))

(defn part-2
  [input]
  (do-the-thing input sum-windows))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 7
  (part-1 task-input)                                       ; => 1832

  ;; Part 2
  (part-2 test-input)                                       ; => 5
  (part-2 task-input)                                       ; => 1858

  )
