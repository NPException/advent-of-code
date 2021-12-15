(ns aoc-2015.day-17
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [aoc-utils :as u]))

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


(defn part-1
  [input target]
  (let [size (count input)
        nums (u/pow 2 size)]
    (loop [n 1
           r 0]
      (if (= n nums)
        r
        (recur (inc n)
               (if (= target (sum input size n))
                 (inc r)
                 r))))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input 25)                                    ; => 4
  (part-1 task-input 150)                                   ; => 1304
  (quick-bench (part-1 task-input 150))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (quick-bench (part-2 task-input))

  )
