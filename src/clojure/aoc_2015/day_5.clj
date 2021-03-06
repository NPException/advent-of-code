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


;; Part 2 rules

(defn non-overlapping-pairs?
  [s]
  (->> (partition 2 1 s)
       (map #(apply str %))
       (some #(->> (string/index-of s %)
                   (+ 2)
                   (string/index-of s %)))))

(defn twin-letter-with-gap?
  [s]
  (->> (partition 3 1 s)
       (some (fn [[x _ y]] (= x y)))))



(comment
  ;; Part 1 => 255
  (->> (string/split-lines task-input)
       (filter (u/and-fn only-legal-pairs? twin-letter? three-vowels?))
       count)
  ;; Part 2 => 55
  (->> (string/split-lines task-input)
       (filter (u/and-fn non-overlapping-pairs? twin-letter-with-gap?))
       count)
  )
