(ns aoc-2015.day-19
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [aoc-utils :as u]))

;; --- Day 19:  --- https://adventofcode.com/2015/day/19

(def task-input (u/slurp-resource "inputs/aoc_2015/day-19.txt"))

(def test-input-1 "H => HO\nH => OH\nO => HH\n\nHOH")
(def test-input-2 "H => HO\nH => OH\nO => HH\n\nHOHOHO")



(defn part-1
  [input]
  )


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input-1)                                     ; => 4
  (part-1 test-input-2)                                     ; => 7
  (part-1 task-input)                                       ; =>
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-1)                                     ; =>
  (part-2 test-input-2)                                     ; =>
  (part-2 task-input)                                       ; =>
  (quick-bench (part-2 task-input))

  )
