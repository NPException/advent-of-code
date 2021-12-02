(ns aoc-2021.day-2
  (:use [criterium.core])
  (:require [aoc-utils :as u]
            [clojure.edn :as edn]))

;; --- Day 2: Dive! --- https://adventofcode.com/2021/day/2

(def task-input (edn/read-string (str "[" (u/slurp-resource "inputs/aoc_2021/day-2.txt") "]")))

(def test-input (edn/read-string "[ forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2 ]"))


(defn navigate
  [input commands]
  (let [ship {:x 0 :y 0 :aim 0}]
    (->> (partition 2 input)
         (reduce
           (fn [shp [command arg]]
             ((commands command) shp arg))
           ship)
         ((juxt :x :y))
         (apply *))))


(defn part-1
  [input]
  (navigate input
            {'forward #(update %1 :x + %2)
             'down #(update %1 :y + %2)
             'up #(update %1 :y - %2)}))


(defn part-2
  [input]
  (navigate input
            {'forward #(-> (update %1 :x + %2)
                           (update :y + (* %2 (:aim %1))))
             'down #(update %1 :aim + %2)
             'up #(update %1 :aim - %2)}))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 150
  (part-1 task-input)                                       ; => 1670340

  ;; Part 2
  (part-2 test-input)                                       ; => 900
  (part-2 task-input)                                       ; => 1954293920

  )
