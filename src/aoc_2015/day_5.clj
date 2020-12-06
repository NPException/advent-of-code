(ns aoc-2015.day-5
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 5: Doesn't He Have Intern-Elves For This? --- https://adventofcode.com/2015/day/5

(def task-input
  (u/slurp-resource "inputs/aoc_2015/day-5.txt"))


;; Part 1 rules

(defn three-vowels?
  [s]
  (->> (map (frequencies s) "aeiou")
       (filter some?)
       (apply +)
       (<= 3)))

(defn twin-letter?
  [s]
  (->> (partition 2 1 s)
       (some #(apply = %))))

(defn only-legal-pairs?
  [s]
  (->> (partition 2 1 s)
       (map #(apply str %))
       (not-any? #{"ab" "cd" "pq" "xy"})))




(comment
  ;; Part 1
  (->> (string/split-lines task-input)
       (filter #(and (only-legal-pairs? %)
                     (twin-letter? %)
                     (three-vowels? %)))
       count)
  ;; Part 2

  )
