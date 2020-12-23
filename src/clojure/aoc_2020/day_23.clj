(ns aoc-2020.day-23
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 23: Crab Cups --- https://adventofcode.com/2020/day/23

(def task-input (u/slurp-resource "inputs/aoc_2020/day-23.txt"))

(def test-input "389125467")


(defn parse-input
  [input]
  (->> input
       (map str)
       (map u/parse-long)))


(defn dec-cup
  ([max-num c]
   (let [i (dec c)]
     (if (< i 1) max-num i)))
  ([max-num c picked-set]
   (loop [c (dec-cup max-num c)]
     (if (picked-set c)
       (recur (dec-cup max-num c))
       c))))


(defn move
  [max-num [current cups]]
  (let [cups (concat cups cups)
        picked (->> cups
                    (drop-while #(not= % current))
                    (drop 1)
                    (take 3))
        picked-set (set picked)
        destination (dec-cup max-num current picked-set)
        new-cups (->> cups
                      (remove picked-set)
                      (drop-while #(not= % destination))
                      (drop 1)
                      (concat picked)
                      (take max-num))
        next-cup (->> new-cups
                      (drop-while #(not= % current))
                      second)]
    [next-cup (vec new-cups)]))


(defn make-moves
  [max-num nums]
  (->> [(first nums) nums]
       (iterate #(move max-num %))
       (drop (* 10 (inc max-num)))
       first
       second))

(defn after-1
  [nums]
  (->> (concat nums nums)
       (drop-while #(not= 1 %))
       (drop 1)
       (take 8)))


(defn part-1
  [input]
  (->> (parse-input input)
       (make-moves 9)
       after-1
       string/join))


(defn part-2
  [input]
  (->> (parse-input input)
       (#(concat % (range 10 1000001)))
       (make-moves 1000000)
       after-1
       (take 2)
       (apply *)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 67384529
  (part-1 task-input)                                       ; => 45798623

  ;; Part 2
  (part-2 test-input)                                       ; => 149245887792
  (part-2 task-input)                                       ; =>

  )
