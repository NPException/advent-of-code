(ns aoc-2020.day-11
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 11: Seating System --- https://adventofcode.com/2020/day/11

(def task-input (u/slurp-resource "inputs/aoc_2020/day-11.txt"))

(def test-input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")


;; for debugging
(defn print-seats
  [seats]
  (doseq [row seats]
    (println (apply str row))))


(defn parse-seats
  ;; returns a map of seat coordinates to occupied state (initially false)
  [input]
  (->> input
       string/split-lines
       (mapv vec)))


(def neighbour-offsets
  (vec
    (for [y (range -1 2)
          x (range -1 2)
          :when (not= y x 0)]
      [x y])))


(defn count-neighbours
  "Counts occupied seats adjacent to the given coordinates"
  [seats x y]
  (transduce
    (map (fn [[ox oy]]
           (if (= \# (get-in seats [(+ y oy) (+ x ox)]))
             1
             0)))
    +
    neighbour-offsets))


(defn process-seat
  [seats y x state]
  (if (= state \.)
    state
    (let [neighbours (count-neighbours seats x y)]
      (or (and (= state \L)
               (= neighbours 0)
               \#)
          (and (= state \#)
               (>= neighbours 4)
               \L)
          state))))


(defn process-row
  [seats y row]
  (into []
        (map-indexed (partial process-seat seats y))
        row))


(defn process-seats
  [seats]
  (into []
        (map-indexed (partial process-row seats))
        seats))


(defn part-1
  [input]
  (->> (parse-seats input)
       (iterate process-seats)
       (partition 2 1)
       (take-while #(apply not= %))
       last
       second
       (apply concat)
       (filter #(= % \#))
       count))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 37
  (part-1 task-input)                                       ; => 2275

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
