(ns aoc-2015.day-1
  (:require [aoc-utils :as u]))

;; --- Day 1:  --- https://adventofcode.com/2015/day/1

(def task-input
  (u/slurp-resource "inputs/aoc_2015/day-1.txt"))


(defn up-down
  [c]
  (if (= c \() 1 -1))


;; Part 1
(defn determine-floor
  [input]
  (apply + (map up-down input)))


;; Part 2
(defn find-basement-instruction
  [input]
  (->> input
       (map up-down)
       (reduce
         (fn [[i floor] command]
           (if (= floor -1)
             (reduced [i])
             [(inc i) (+ floor command)]))
         [0 0])))



(comment
  ;; Part 1
  (determine-floor task-input)
  ;; Part 2
  (find-basement-instruction task-input)
  )
