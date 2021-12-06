(ns aoc-2021.day-6
  (:use [criterium.core])
  (:require [aoc-utils :as u]))

;; --- Day 6: Lanternfish --- https://adventofcode.com/2021/day/6

(def task-input (read-string (str "[" (u/slurp-resource "inputs/aoc_2021/day-6.txt") "]")))

(def test-input (read-string "[3,4,3,1,2]"))

(defn update-1-day
  [new-fish timer num]
  (if (zero? timer)
    (-> (update new-fish 6 + num)                           ;; reset timer on spawn
        (update 8 + num))                                   ;; spawn new fish
    (update new-fish (dec timer) + num)))                   ;; decrease timer on other fish

(defn count-fish
  [days fish]
  (if (zero? days)
    (apply + fish)
    (recur (dec days) (reduce-kv update-1-day [0 0 0 0 0 0 0 0 0] fish))))

(defn calculate
  [input days]
  (count-fish days (frequencies input)))


(comment
  ;; Part 1
  (calculate test-input 80)                                   ;; 5934
  (calculate task-input 80)                                   ;; 365862

  ;; Part 2
  (calculate test-input 256)                                  ;; 26984457539
  (calculate task-input 256)                                  ;; 1653250886439
  (quick-bench (calculate task-input 256))
  ;
  )
