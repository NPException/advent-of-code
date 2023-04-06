(ns aoc-2020.day-9
  (:require [aoc-utils :as u]
            [clojure.string :as string]))

;; --- Day 9: Encoding Error --- https://adventofcode.com/2020/day/9

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-9.txt"))

(def input-numbers (->> task-input
                        string/split-lines
                        (mapv parse-long)))

;; part 1 functions

(defn invalid?
  [[n preamble]]
  (empty?
    (for [x preamble
          y preamble
          :when (and (not= x y)
                     (= n (+ x y)))]
      :dummy)))


(defn validation-groups
  [numbers preamble-size]
  (->> (u/vpartition (inc preamble-size) 1 numbers)
       (map (juxt peek pop))))


;; part 2 functions

(defn find-sum-group
  [numbers target]
  (loop [remaining (drop 2 numbers)
         window (vec (take 2 numbers))]
    (let [sum (apply + window)]
      (cond
        (= sum target) window
        (< sum target) (recur (next remaining)
                              (conj window (first remaining)))
        (> sum target) (recur remaining
                              (vec (rest window)))))))



(comment
  ;; Part 1 => 32321523
  (->> (validation-groups input-numbers 25)
       (filter invalid?)
       ffirst)
  ;; Part 2 => 4794981
  (->> (find-sum-group input-numbers 32321523)
       (apply (juxt min max))
       (apply +))
  )
