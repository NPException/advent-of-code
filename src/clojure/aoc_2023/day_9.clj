(ns aoc-2023.day-9
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 9: Mirage Maintenance ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-9.txt"))

(def test-input "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv u/read-as-vector)))

(defn difference
  [[a b]]
  (- b a))

(defn find-next-number
  [nums]
  (->> nums
       (iterate #(mapv difference (partition 2 1 %)))
       (take-while #(not (every? zero? %)))
       (mapv peek)
       rseq
       (reduce + 0)))


(defn part-1
  [input]
  (->> (parse-input input)
       (mapv find-next-number)
       (apply +)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 114
  (part-1 task-input)                                       ; => 1884768153
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
