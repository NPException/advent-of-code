(ns aoc-2022.day-2-alt
  (:require [aoc-utils :as u]
            [criterium.core :as crit]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 2: Rock Paper Scissors ---

(defn parse-input
  [input]
  (->> (u/read-as-vector input)
       (mapv '{A 0, B 1, C 2, X 0, Y 1, Z 2})
       (u/vpartition 2)
       vec))


(def task-input (parse-input (u/slurp-resource "inputs/aoc_2022/day-2.txt")))

(def test-input (parse-input "A Y\nB X\nC Z"))


(def loses-to {1 0, 2 1, 0 2})

(def wins-against (set/map-invert loses-to))

(defn build-scoring
  [determine-move]
  (->> (for [opp (range 3)
             me  (range 3)
             :let [move (determine-move opp me)]]
         [[opp me]
          (+ (inc move)
             (cond (= opp move) 3
                   (= opp (loses-to move)) 6
                   :else 0))])
       (apply concat)
       (apply array-map)))


(def part-1-scoring (build-scoring #(do %2)))

(def part-2-scoring (build-scoring (fn [opp goal]
                                     (u/fn-> {0 loses-to
                                              1 identity
                                              2 wins-against}
                                             goal
                                             opp))))


(defn calculate
  [scoring input]
  (transduce (map scoring) u/+l 0 input))


(def part-1 (partial calculate part-1-scoring))
(def part-2 (partial calculate part-2-scoring))

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
