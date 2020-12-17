(ns aoc-2015.day-8
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 8: Matchsticks --- https://adventofcode.com/2015/day/8

(def task-input (u/slurp-resource "inputs/aoc_2015/day-8.txt"))


(defn char-from-hex
  [h1 h2]
  (char (Integer/parseInt (str h1 h2) 16)))

(defn un-escape
  [line]
  (loop [result []
         [c & [x & [h1 h2 & after-hex :as after-escape] :as all-remaining]] (subs line 1 (dec (count line)))]
    (cond
      (nil? c)    (string/join result)
      (not= c \\) (recur (conj result c) all-remaining)
      (= x \x)    (recur (conj result (char-from-hex h1 h2)) after-hex)
      :else       (recur (conj result x) after-escape))))


(defn part-1
  [input]
  (->> (string/split-lines input)
       (map (juxt count (comp count un-escape)))
       (map (partial apply -))
       (apply +)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 1350

  ;; Part 2
  (part-2 task-input)                                       ; =>

  )
