(ns aoc-2015.day-6
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

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
  (letfn [(target? [i]
            (and (<= x1 (mod i 1000) x2)
                 (<= y1 (quot i 1000) y2)))]
    (case action
      "turn on" (into []
                      (map-indexed #(if (target? %1) true %2))
                      data)
      "turn off" (into []
                       (map-indexed #(if (target? %1) false %2))
                       data)
      "toggle" (into []
                     (map-indexed #(if (target? %1) (not %2) %2))
                     data))))


(defn part-1
  [input]
  (let [start-data (vec (repeat (* 1000 1000) false))
        final-data (reduce
                     toggle-lights
                     start-data
                     (parse-instructions input))]
    (->> final-data
         (filter true?)
         count)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 task-input)

  ;; Part 2
  (part-2 task-input)

  )
