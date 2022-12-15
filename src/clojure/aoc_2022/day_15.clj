(ns aoc-2022.day-15
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 15: Beacon Exclusion Zone ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-15.txt"))

(def test-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3")


(defn parse-input
  [input]
  (->> (str/split input #"\n|x=|, y=|: ")
       (keep parse-long)
       (vec)
       (u/vpartition 4)))


(defn prepare-beacon
  "takes sensor x/y, and beacon offset x/y, and returns a vector of the sensor range and a
  predicate that returns true if a given coordinate is within the sensor range, but cannot contain a beacon."
  [^long sx ^long sy ^long bx ^long by]
  (let [range (+ (abs (- bx sx))
                 (abs (- by sy)))]
    [range
     (fn [^long x ^long y]
       (and (<= (+ (abs (- x sx))
                   (abs (- y sy)))
              range)
            (not (and (== x bx) (== y by)))))]))


(defn part-1
  [input y]
  (let [[min-x max-x preds] (->> (parse-input input)
                                 (reduce
                                   (fn [[min-x max-x preds] [sx sy bx by]]
                                     (let [[distance pred] (prepare-beacon sx sy bx by)]
                                       [(min min-x (- sx distance))
                                        (max max-x (+ sx distance))
                                        (conj preds pred)]))
                                   [Long/MAX_VALUE
                                    Long/MIN_VALUE
                                    []]))
        mega-pred (apply u/or-fn preds)]
    (->> (range min-x (inc max-x))
         (u/count-matching (fn [x] (mega-pred x y))))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input 10)                                    ; => 26
  (part-1 task-input 2000000)                               ; => 4907780
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 56000011
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
