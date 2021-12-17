(ns aoc-2015.day-18
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 18: Like a GIF For Your Yard --- https://adventofcode.com/2015/day/18

(def task-input (u/slurp-resource "inputs/aoc_2015/day-18.txt"))

(def test-input ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####..")

(defn parse-grid
  [input]
  (let [lines (str/split-lines input)
        size (count lines)]
    [size
     (mapv #(if (= % \#) true false) (apply concat lines))]))


(defn at
  [^long size grid ^long x ^long y]
  (and (< -1 x size) (< -1 y size)
       (nth grid (+ x (* y size)) false)))

(defn active-neighbours
  [^long size grid ^long i]
  (let [x (rem i size)
        y (quot i size)]
    (cond-> 0
      (at size grid (dec x) (dec y)) inc
      (at size grid x (dec y)) inc
      (at size grid (inc x) (dec y)) inc
      (at size grid (dec x) y) inc
      (at size grid (inc x) y) inc
      (at size grid (dec x) (inc y)) inc
      (at size grid x (inc y)) inc
      (at size grid (inc x) (inc y)) inc)))

(defn simulate-cell
  [size grid i on?]
  (let [n (active-neighbours size grid i)]
    (or (and on? (or (= n 2) (= n 3)))
        (and (not on?) (= n 3)))))


(defn print-grid
  [^long size grid]
  (dotimes [y size]
    (dotimes [x size]
      (print (if (grid (+ x (* y size))) \# \.)))
    (println))
  (println))


(defn simulate-grid
  [size grid steps simulate-cell]
  (loop [i 0, grid grid]
    #_(print-grid size grid)
    (if (= i steps)
      (count (filter true? grid))
      (recur (inc i) (vec (map-indexed #(simulate-cell size grid %1 %2) grid))))))


(defn part-1
  [input steps]
  (let [[size grid] (parse-grid input)]
    (simulate-grid size grid steps simulate-cell)))


(defn part-2
  [input steps]
  (let [[^long size grid] (parse-grid input)
        c1 0, c2 (dec size), c3 (* size (dec size)), c4 (dec (* size size))]
    (simulate-grid
      size
      (-> (assoc grid c1 true)
          (assoc c2 true)
          (assoc c3 true)
          (assoc c4 true))
      steps
      (fn [^long size grid ^long i on?]
        (or (= i c1) (= i c2) (= i c3) (= i c4)
            (simulate-cell size grid i on?))))))


(comment
  ;; Part 1
  (part-1 test-input 4)                                     ; => 4
  (part-1 task-input 100)                                   ; => 814
  (quick-bench (part-1 task-input 100))

  ;; Part 2
  (part-2 test-input 5)                                     ; => 17
  (part-2 task-input 100)                                   ; => 924
  (quick-bench (part-2 task-inpu 100))

  )
