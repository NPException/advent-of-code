(ns aoc-2019.day-2
  (:require [aoc-utils :as u]
            [aoc-2019.intcode-interpreter :as intcode]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 2: 1202 Program Alarm ---

(def task-input (u/slurp-resource "inputs/aoc_2019/day-2.txt"))

(def test-input "1,9,10,3,2,3,11,0,99,30,40,50  ,30")       ;; modified to work with the 12/2 replacements


(defn part-1
  [input]
  (-> (intcode/create-state input)
      (assoc-in [:mem 1] 12)
      (assoc-in [:mem 2] 2)
      (intcode/run-program)
      (:mem)
      (nth 0)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1600
  (part-1 task-input)                                       ; => 3760627
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
