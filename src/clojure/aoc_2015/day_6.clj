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


;; part 2
(defn adjust-lights
  [data [action x1 y1 x2 y2]]
  (letfn [(target? [i]
            (and (<= x1 (mod i 1000) x2)
                 (<= y1 (quot i 1000) y2)))]
    (case action
      "turn on" (into []
                      (map-indexed #(if (target? %1) (inc %2) %2))
                      data)
      "turn off" (into []
                       (map-indexed #(if (and (target? %1) (> %2 0)) (dec %2) %2))
                       data)
      "toggle" (into []
                     (map-indexed #(if (target? %1) (+ %2 2) %2))
                     data))))

(defn part-2
  [input]
  (let [start-data (vec (repeat (* 1000 1000) 0))
        final-data (reduce
                     adjust-lights
                     start-data
                     (parse-instructions input))]
    (reduce + final-data)))


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 400410

  ;; Part 2
  (part-2 task-input)                                       ; => 15343601

  )
