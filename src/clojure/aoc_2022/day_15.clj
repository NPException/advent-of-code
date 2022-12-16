(ns aoc-2022.day-15
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]
            [debug-utils :as d]))

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


(defn prepare-beacon-1
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
  (let [[^long min-x, ^long max-x, preds] (->> (parse-input input)
                                               (reduce
                                                 (fn [[^long min-x, ^long max-x, preds]
                                                      [^long sx, ^long sy, bx by]]
                                                   (let [[^long distance pred] (prepare-beacon-1 sx sy bx by)]
                                                     [(min min-x (- sx distance))
                                                      (max max-x (+ sx distance))
                                                      (conj preds pred)]))
                                                 [Long/MAX_VALUE
                                                  Long/MIN_VALUE
                                                  []]))
        mega-pred (apply u/or-fn preds)]
    (->> (range min-x (inc max-x))
         (u/count-matching (fn [x] (mega-pred x y))))))


(defn prepare-beacon-2
  "takes sensor x/y, and beacon offset x/y, and returns a function that for any given y coordinate
  will return a region of x that's within the sensor's reach."
  [^long size [^long sx ^long sy ^long bx ^long by]]
  (let [range (+ (abs (- bx sx))
                 (abs (- by sy)))]
    (fn [^long y]
      (let [d (- range (abs (- y sy)))]
        (when (>= d 0)
          [(max 0 (- sx d)) (min size (+ sx d))])))))


(defn find-beacon
  [range-fns y]
  (let [result (reduce
                 (fn [[_ ^long b1 :as current] [^long a2 ^long b2 :as next]]
                   (cond
                     ;; next region ends within the current
                     (<= b2 b1) current
                     ;; next region overlapping or adjacent
                     (<= a2 (inc b1)) next
                     ;; found the coordinate
                     :else (reduced (inc b1))))
                 (sort (u/keepv #(% y) range-fns)))]
    (when (int? result)
      [result y])))


(defn part-2
  [input ^long size]
  (let [update-progress (d/spawn-progress-bar "2022 Day 15 - Part 2" 0 size)
        range-fns       (->> (parse-input input)
                             (mapv #(prepare-beacon-2 size %)))
        [x y] (->> (range (inc size))
                   (keep #(find-beacon range-fns (update-progress %)))
                   (first))]
    (-> (* x 4000000)
        (+ y))))


(comment
  ;; Part 1
  (part-1 test-input 10)                                    ; => 26
  (part-1 task-input 2000000)                               ; => 4907780
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input 20)                                    ; => 56000011
  (part-2 task-input 4000000)                               ; => 13639962836448
  (crit/quick-bench (part-2 task-input))

  )
