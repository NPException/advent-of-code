(ns aoc-2020.day-5
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 5: Binary Boarding --- https://adventofcode.com/2020/day/5

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-5.txt"))


(defn parse-seat-id
  [seat]
  (-> (string/replace seat #"." {"F" "0", "B" "1", "L" "0", "R" "1"})
      (Integer/parseInt 2)))


(defn find-highest-seat-id
  [input]
  (->> (string/split-lines input)
       (map parse-seat-id)
       (apply max)))


(defn find-my-seat
  [input]
  (->> (string/split-lines input)
       (map parse-seat-id)
       sort
       (#(interleave % (rest %)))
       (partition 2)
       (filter (fn [[a b]] (not= (inc a) b)))
       ffirst
       inc))


(comment
  ;; Part 1
  (find-highest-seat-id task-input)
  ;; Part 2
  (find-my-seat task-input)
  )
