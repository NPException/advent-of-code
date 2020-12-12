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


; part 1

(defn toggle-lights
  [data [action x1 y1 x2 y2]]
  (let [x-end (inc x2)
        y-end (inc y2)]
    (case action
      "turn on" (dotimes [oy (- y-end y1)]
                  (.set ^BitSet (data (+ y1 oy)) ^int x1 ^int x-end))
      "turn off" (dotimes [oy (- y-end y1)]
                   (.clear ^BitSet (data (+ y1 oy)) ^int x1 ^int x-end))
      "toggle" (dotimes [oy (- y-end y1)]
                 (.flip ^BitSet (data (+ y1 oy)) ^int x1 ^int x-end))))
  data)


(defn part-1
  [input]
  (let [start-data (vec (repeatedly 1000 #(BitSet. 1000)))
        final-data (reduce
                     toggle-lights
                     start-data
                     (parse-instructions input))]
    (->> final-data
         (map #(.cardinality ^BitSet %))
         (apply +))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 400410

  ;; Part 2
  (part-2 task-input)

  )
