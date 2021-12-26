(ns aoc-2021.day-25
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 25: Sea Cucumber --- https://adventofcode.com/2021/day/25

(def task-input (u/slurp-resource "inputs/aoc_2021/day-25.txt"))

(def test-input "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>")

(def ^:const right 1)
(def ^:const down 2)
(def ^:const added 3)
(def ^:const removed 4)

(defn parse-input
  [input]
  (let [lines (str/split-lines input)
        chars (apply concat lines)
        width (count (first lines))]
    [width
     (byte-array (map #(case % \> right, \v down, (byte 0)) chars))]))


(defn print-grid
  [^bytes grid width]
  (let [size   (count grid)
        height (/ size width)]
    (dotimes [y height]
      (dotimes [x width]
        (print (case (aget grid (+ (* y width) x))
                 1 \>
                 2 \v
                 \.)))
      (println))
    (println)))


(defn clear!
  [^bytes grid val]
  (let [zero (byte 0)
        val  (byte val)]
    (dotimes [pos (alength grid)]
      (case (aget grid pos)
        3 (aset grid pos val)
        4 (aset grid pos zero)
        nil))))

(defn update-right!
  [^bytes grid ^long width]
  (let [size  (alength grid)
        right (byte right)
        added (byte added)
        removed (byte removed)]
    (loop [pos      0
           changed? false]
      (if (= pos size)
        (do (clear! grid right)
            changed?)
        (if (= right (aget grid pos))
          (let [x   (rem pos width)
                y   (quot pos width)
                new (+ (* width y) (rem (inc x) width))]
            (if (zero? (aget grid new))
              (do (aset grid new added)
                  (aset grid pos removed)
                  (recur (inc pos) true))
              (recur (inc pos) changed?)))
          (recur (inc pos) changed?))))))


(defn update-down!
  [^bytes grid ^long width]
  (let [size  (alength grid)
        down  (byte down)
        added (byte added)
        removed (byte removed)]
    (loop [pos      0
           changed? false]
      (if (= pos size)
        (do (clear! grid down)
            changed?)
        (if (= down (aget grid pos))
          (let [new (rem (+ pos width) size)]
            (if (zero? (aget grid new))
              (do (aset grid new added)
                  (aset grid pos removed)
                  (recur (inc pos) true))
              (recur (inc pos) changed?)))
          (recur (inc pos) changed?))))))


(defn part-1
  [input]
  (let [[width grid] (parse-input input)]
    #_(print-grid grid width)
    (loop [step 1]
      (let [changed1 (update-right! grid width)
            changed2 (update-down! grid width)]
        #_(print-grid grid width)
        (if (and (not changed1) (not changed2))
          step
          (recur (inc step)))))))



(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 58
  (part-1 task-input)                                       ; => 380
  (quick-bench (part-1 task-input))

  )
