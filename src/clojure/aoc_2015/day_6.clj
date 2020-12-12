(ns aoc-2015.day-6
  (:require [clojure.string :as string]
            [aoc-utils :as u])
  (:import [java.util BitSet]))

;; --- Day 6: Probably a Fire Hazard --- https://adventofcode.com/2015/day/6

(def task-input (u/slurp-resource "inputs/aoc_2015/day-6.txt"))

(defn try-long
  [x]
  (or (u/parse-long x) x))

(defn parse-instructions
  [input]
  (->> (string/split-lines input)
       (map #(re-matches #"(turn on|turn off|toggle) (\d{1,3}),(\d{1,3}) through (\d{1,3}),(\d{1,3})" %))
       (map rest)
       (mapv #(mapv try-long %))))


(defn change-bits
  [^BitSet bits
   ^String action
   ^long from
   ^long to]
  (case action
    "turn on" (.set bits from to)
    "turn off" (.clear bits from to)
    "toggle" (.flip bits from to)))


(defn change-lights
  [grid [action x1 y1 x2 y2]]
  (let [x-end (inc x2)]
    (dotimes [oy (- (inc y2) y1)]
      (change-bits (grid (+ y1 oy)) action x1 x-end))))


;; grid is a vector of java BitSets (evil mutation in place)
(defn create-grid
  []
  (into [] (repeatedly 1000 #(BitSet. 1000))))


(defn part-1
  [input]
  (let [grid (create-grid)]
    (doseq [ins (parse-instructions input)]
      (change-lights grid ins))
    (->> grid
         (map (fn [^BitSet bitset]
                (.cardinality bitset)))
         (apply +))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 task-input)

  ;; Part 2
  (part-2 task-input)

  )
