(ns aoc-2015.day-17
  (:use [criterium.core])
  (:require [aoc-utils :as u]))

;; --- Day 17: No Such Thing as Too Much --- https://adventofcode.com/2015/day/17

(def task-input (read-string (str "[" (u/slurp-resource "inputs/aoc_2015/day-17.txt") "]")))

(def test-input [20 15 10 5 5])


(defn sum
  [numbers size n]
  (loop [i 0
         r 0]
    (if (= i size)
      r
      (recur (inc i)
             (if (bit-test n i)
               (+ r ^long (nth numbers i))
               r)))))

(defn find-containers
  [input target]
  (let [size (count input)]
    (->> (range (u/pow 2 size))
         (filter #(= target (sum input size %))))))


(defn part-1
  [input target]
  (count (find-containers input target)))


(defn num-containers
  [size container-bits]
  (->> (range size)
       (filter #(bit-test container-bits %))
       count))

(defn part-2
  [input target]
  (let [size (count input)]
    (->> (find-containers input target)
         (map #(num-containers size %))
         sort
         (partition-by identity)
         first
         count)))


(comment
  ;; Part 1
  (part-1 test-input 25)                                    ; => 4
  (part-1 task-input 150)                                   ; => 1304
  (quick-bench (part-1 task-input 150))

  ;; Part 2
  (part-2 test-input 25)                                    ; => 3
  (part-2 task-input 150)                                   ; => 18
  (quick-bench (part-2 task-input 150))

  )
