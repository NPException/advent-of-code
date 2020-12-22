(ns aoc-2020.day-22
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 22: Crab Combat --- https://adventofcode.com/2020/day/22

(def task-input (u/slurp-resource "inputs/aoc_2020/day-22.txt"))

(def test-input "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10")


(defn parse-input
  [input]
  (->> (string/split input #"\n\n")
       (map #(read-string (str "[" (second (string/split % #":\n")) "]")))))


(defn play-round
  [[[card-1 & deck-1] [card-2 & deck-2]]]
  (if (> card-1 card-2)
    [(into (vec deck-1) [card-1 card-2])
     deck-2]
    [deck-1
     (into (vec deck-2) [card-2 card-1])]))


(defn part-1
  [input]
  (->> (parse-input input)
       (iterate play-round)
       (drop-while (fn [[deck-1 deck-2]]
                     (and (seq deck-1) (seq deck-2))))
       first
       (filter seq)
       first
       reverse
       (map-indexed #(* (inc %1) %2))
       (apply +)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 306
  (part-1 task-input)                                       ; => 32629

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
