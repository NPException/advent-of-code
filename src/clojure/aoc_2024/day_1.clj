(ns aoc-2024.day-1
  (:require [aoc-utils :as u]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 1: Historian Hysteria ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-1.txt"))

(def test-input "3   4\n4   3\n2   5\n1   3\n3   9\n3   3")

(defn parse-input
  [text]
  (->> (u/read-as-vector text)
       (u/vpartition 2)
       (u/transpose)))


(defn part-1
  [input]
  (->> (parse-input input)
       (map sort)
       (apply map -)
       (map abs)
       (apply +)))


(defn part-2
  [input]
  (let [[left right] (parse-input input)
        freq (frequencies right)]
    (->> left
         (map #(* % (freq % 0)))
         (apply +))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 11
  (part-1 task-input)                                       ; => 936063
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 31
  (part-2 task-input)                                       ; => 23150395
  (crit/quick-bench (part-2 task-input))

  )
