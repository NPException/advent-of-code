(ns aoc-2022.day-4
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 4: Camp Cleanup ---

;; parsing takes ~1.7ms
(defn parse-input
  [input]
  (->> (str/replace input \- \,)
       (u/read-as-vector)
       (u/vpartition 4)))


(def task-input (parse-input (u/slurp-resource "inputs/aoc_2022/day-4.txt")))

(def test-input (parse-input "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"))


(defn fully-contained?
  [[a1 a2 b1 b2]]
  (or (<= a1 b1 b2 a2)
      (<= b1 a1 a2 b2)))


(defn part-1
  [input]
  (u/count-matching fully-contained? input))


(defn overlap?
  [[a1 a2 b1 b2]]
  (or (<= a1 b1 a2)                                         ;; a1-b1-a2-b2  or  a1-b1-b2-a2
      (<= b1 a1 b2)))                                       ;; b1-a1-b2-a2  or  b1-a1-a2-b2


(defn part-2
  [input]
  (u/count-matching overlap? input))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 2
  (part-1 task-input)                                       ; => 487
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 4
  (part-2 task-input)                                       ; => 849
  (crit/quick-bench (part-2 task-input))

  )
