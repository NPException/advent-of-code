(ns aoc-2020.day-15
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 15: Rambunctious Recitation --- https://adventofcode.com/2020/day/15

(def task-input (u/slurp-resource "inputs/aoc_2020/day-15.txt"))

(def test-input "0,3,6")


(defn build-iteration-seed
  [seed-nums]
  [(last seed-nums)
   (count seed-nums)
   (->> (map-indexed #(vector %2 %1) seed-nums)
        (into {})
        transient)])

(defn calculate-next-number
  [[prev index seen]]
  (let [prev-index (dec index)]
    [(- prev-index (seen prev prev-index))
     (inc index)
     (assoc! seen prev prev-index)]))

(defn gen-numbers
  [seed-nums]
  (->> (build-iteration-seed seed-nums)
       (iterate calculate-next-number)
       (drop 1)
       (map first)
       (concat seed-nums)))


(defn find-spoken-number
  [input n]
  (->> (string/split input #",")
       (map u/parse-long)
       gen-numbers
       (take n)
       last))


(defn part-1
  [input]
  (find-spoken-number input 2020))

(defn part-2
  [input]
  (find-spoken-number input 30000000))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 436
  (part-1 task-input)                                       ; => 203

  ;; Part 2
  (part-2 test-input)                                       ; => 175594
  (part-2 task-input)                                       ; => 9007186

  )
