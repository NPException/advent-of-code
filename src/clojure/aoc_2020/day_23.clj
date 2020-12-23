(ns aoc-2020.day-23
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 23: Crab Cups --- https://adventofcode.com/2020/day/23

(def task-input (u/slurp-resource "inputs/aoc_2020/day-23.txt"))

(def test-input "389125467")


(defn dec-cup
  ([c]
   (let [i (dec (int c))]
     (if (< i 49) \9 (char i))))
  ([c picked-set]
   (loop [c (dec-cup c)]
     (if (picked-set c)
       (recur (dec-cup c))
       c))))


(defn move
  [[current cups]]
  (let [cups (concat cups cups)
        picked (->> cups
                    (drop-while #(not= % current))
                    (drop 1)
                    (take 3))
        picked-set (set picked)
        destination (dec-cup current picked-set)
        new-cups (->> cups
                      (remove picked-set)
                      (drop-while #(not= % destination))
                      (drop 1)
                      (concat picked)
                      (take 9))
        next-cup (->> new-cups
                      (drop-while #(not= % current))
                      second)]
    [next-cup new-cups]))


(defn make-moves
  [input]
  (->> [(first input) input]
       (iterate move)
       (drop 100)
       first
       second))

(defn after-1
  [nums]
  (->> (concat nums nums)
       (drop-while #(not= \1 %))
       (drop 1)
       (take 8)
       string/join))


(defn part-1
  [input]
  (after-1 (make-moves input)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 67384529
  (part-1 task-input)                                       ; => 45798623

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
