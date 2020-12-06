(ns aoc-2015.day-3
  (:require [aoc-utils :as u]))

;; --- Day 3:  --- https://adventofcode.com/2015/day/3

(def task-input
  (u/slurp-resource "inputs/aoc_2015/day-3.txt"))


(defn move
  [[x y] direction]
  (case direction
    \< [(dec x) y]
    \> [(inc x) y]
    \^ [x (dec y)]
    \v [x (inc y)]))

(defn lucky-houses
  [instructions]
  (->> instructions
       (reduce
         (fn [[visited location] direction]
           (let [new-loc (move location direction)]
             [(conj visited new-loc) new-loc]))
         [#{[0 0]} [0 0]])
       first))



(comment
  ;; Part 1
  (count (lucky-houses task-input))
  ;; Part 2
  (count
    (reduce
      conj
      (lucky-houses (take-nth 2 task-input))
      (lucky-houses (take-nth 2 (rest task-input)))))
  )
