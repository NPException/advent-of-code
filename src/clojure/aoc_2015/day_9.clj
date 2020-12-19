(ns aoc-2015.day-9
  (:require [clojure.string :as string]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 9: All in a Single Night --- https://adventofcode.com/2015/day/9

(def task-input (u/slurp-resource "inputs/aoc_2015/day-9.txt"))

(def test-input "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141")


(defn with-all-keys
  [m]
  [(apply set/union (keys m)) m])

(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map #(string/split % #" to | = "))
       (map (fn [[from to dist]]
              [#{from to} (u/parse-long dist)]))
       (into {})
       with-all-keys))


(defn find-shortest-distance
  [min-max distances from destination-set]
  (if (= 1 (count destination-set))
    (distances #{from (first destination-set)})
    (->> destination-set
         (map #(+ (distances #{from %})
                  (find-shortest-distance
                    min-max
                    distances
                    %
                    (disj destination-set %))))
         (apply min-max))))


(defn solve
  [input min-max]
  (let [[towns distances] (parse-input input)]
    (->> towns
         (map #(find-shortest-distance min-max distances % (disj towns %)))
         (apply min-max))))


(defn part-1
  [input]
  (solve input min))


(defn part-2
  [input]
  (solve input max))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 605
  (part-1 task-input)                                       ; => 251

  ;; Part 2
  (part-2 test-input)                                       ; => 982
  (part-2 task-input)                                       ; => 898

  )
