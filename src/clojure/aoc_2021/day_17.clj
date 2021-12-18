(ns aoc-2021.day-17
  (:use [criterium.core])
  (:require [aoc-utils :as u]))

;; --- Day 17: Trick Shot --- https://adventofcode.com/2021/day/17

(def task-input (u/slurp-resource "inputs/aoc_2021/day-17.txt"))
(def test-input "target area: x=20..30, y=-10..-5")

(defn parse
  [input]
  (->> (rest (re-find #"x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" input))
       (mapv parse-long)))

(defn part-1
  [input]
  (let [[_ _ ^long y1] (parse input)
        max-dy (dec (- y1))]
    (/ (* max-dy (inc max-dy)) 2)))


(defn reaches-x?
  [^long dx ^long xt]
  (loop [dx dx
         x 0]
    (cond
      (or (= dx 0) (= x xt)) (= x xt)
      (> x xt) false
      :else (recur (dec dx) (+ x dx)))))

(defn find-dxs
  [x1 x2]
  (->> (range x1 (inc x2))
       (mapcat (fn [x]
                 (->> (range (inc x))
                      (filter #(reaches-x? % x)))))
       distinct))

(defn valid?
  [x y dx dy x1 x2 y1 y2]
  (and (<= x x2)
       (>= y y1)
       (or (and (<= x1 x x2)
                (<= y1 y y2))
           (recur (+ x dx) (+ y dy)
                  (if (> dx 0) (dec dx) 0)
                  (dec dy)
                  x1 x2 y1 y2))))

(defn part-2
  [input]
  (let [[x1 x2 y1 y2] (parse input)
        dxs (find-dxs x1 x2)
        max-dy (dec (- y1))
        min-dy y1]
    (count (for [dx dxs #_(range 1 (inc x2))
                 dy (range min-dy (inc max-dy))
                 :when (valid? 0 0 dx dy x1 x2 y1 y2)]
             true))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 45
  (part-1 task-input)                                       ; => 3655
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 112
  (part-2 task-input)                                       ; => 1447
  (quick-bench (part-2 task-input))

  )
