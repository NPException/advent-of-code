(ns aoc-2022.day-2
  (:require [aoc-utils :as u]
            [criterium.core :as crit]
            [clojure.set :as set]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 2: Rock Paper Scissors ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-2.txt"))

(def test-input "A Y\nB X\nC Z")


(defn parse-input
  [input]
  (->> (u/read-as-vector input)
       (map '{A 0, B 1, C 2, X 0, Y 1, Z 2})
       (partition 2)))


(def loses-to
  "return the move that would lose against the given move"
  {1 0, 2 1, 0 2})

(def wins-against
  "returns the move that would win against the given move"
  (set/map-invert loses-to))

(defn round-score
  [opp me]
  (cond
    (= opp me) 3
    (= opp (loses-to me)) 6
    :else 0))

(defn score
  [[opp me]]
  (+ (inc me)
     (round-score opp me)))


(defn part-1
  [input]
  (->> (parse-input input)
       (map score)
       (apply +)))


(def determine-move
  {0 loses-to
   1 identity
   2 wins-against})

(defn part-2
  [input]
  (->> (parse-input input)
       (map (fn [[opp goal]]
              [opp ((determine-move goal) opp)]))
       (map score)
       (apply +)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 15
  (part-1 task-input)                                       ; => 9651
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 12
  (part-2 task-input)                                       ; => 10560
  (crit/quick-bench (part-2 task-input))

  )
