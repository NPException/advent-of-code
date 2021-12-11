(ns aoc-2021.day-11
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 11: Dumbo Octopus --- https://adventofcode.com/2021/day/11

(def task-input (u/slurp-resource "inputs/aoc_2021/day-11.txt"))

(def test-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")

(defn parse-grid
  [input]
  (let [lines (str/split-lines input)]
    {:width (count lines)
     :edge (dec (count lines))
     :array (->> (str/join lines)
                 (mapv #(- (int %) 48))
                 int-array)}))

(defn inc!
  [{:keys [^ints array ^long width]} ^long x ^long y]
  (let [i (+ (* y width) x)
        e (aget array i)]
    (and (< e 10) (= 10 (aset array i (inc e))))))

(defn sim-octopus!
  ^long [{:keys [^long edge] :as grid} ^long x ^long y]
  (if-not (inc! grid x y)
    0
    (let [x- (dec x), x+ (inc x)
          x-? (> x 0), x+? (< x edge)
          y- (dec y), y+ (inc y)
          y-? (> y 0), y+? (< y edge)]
      (cond-> 1
        y-? (+ (cond-> (sim-octopus! grid x y-)
                 x-? (+ (sim-octopus! grid x- y-))
                 x+? (+ (sim-octopus! grid x+ y-))))
        x-? (+ (sim-octopus! grid x- y))
        x+? (+ (sim-octopus! grid x+ y))
        y+? (+ (cond-> (sim-octopus! grid x y+)
                 x-? (+ (sim-octopus! grid x- y+))
                 x+? (+ (sim-octopus! grid x+ y+))))))))

(defn refresh-flashed!
  [{:keys [^ints array]}]
  (dotimes [i (alength array)]
    (when (= 10 (aget array i))
      (aset array i 0))))

(defn sim-step!
  ^long [{:keys [^long width ^long edge] :as grid}]
  (refresh-flashed! grid)
  (loop [x 0
         y 0
         flashes 0]
    (if (= y width)
      flashes
      (recur (if (= x edge) 0 (inc x))
             (if (= x edge) (inc y) y)
             (+ flashes (sim-octopus! grid x y))))))


(defn part-1
  [input ^long steps]
  (let [grid (parse-grid input)]
    (loop [flashes 0
           i 0]
      (if (= i steps)
        flashes
        (recur (+ flashes (sim-step! grid)) (inc i))))))


(defn part-2
  [input]
  (let [grid (parse-grid input)
        size (alength ^ints (:array grid))]
    (loop [step 1]
      (if (= size (sim-step! grid))
        step
        (recur (inc step))))))


(comment
  ;; Part 1
  (part-1 test-input 100)                                   ; => 1656
  (part-1 task-input 100)                                   ; => 1625
  (quick-bench (part-1 task-input 100))                     ; => ~ 1.1 ms

  ;; Part 2
  (part-2 test-input)                                       ; => 195
  (part-2 task-input)                                       ; => 244
  (quick-bench (part-2 task-input))                         ; => ~ 2.7 ms

  )
