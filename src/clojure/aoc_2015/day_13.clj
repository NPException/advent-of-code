(ns aoc-2015.day-13
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 13: Knights of the Dinner Table --- https://adventofcode.com/2015/day/13

(def task-input (u/slurp-resource "inputs/aoc_2015/day-13.txt"))

(def test-input "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol.")


(defn parse-line
  [line]
  (let [[_ name op n name-2]
        (re-matches #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\."
                    line)
        n (parse-long n)]
    {name {name-2 (if (= op "gain") n (- n))}}))

(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map parse-line)
       (apply merge-with merge)))


(defn calculate-happiness
  [preferences seating-order]
  (->> (conj (vec seating-order) (first seating-order))
       (partition 2 1)
       (reduce
         #(+ %1
             (get-in preferences %2)
             (get-in preferences (reverse %2)))
         0)))


(defn optimal-seating-score
  [preferences]
  (->> (keys preferences)
       u/permutations
       (map (partial calculate-happiness preferences))
       (apply max)))


(defn part-1
  [input]
  (optimal-seating-score (parse-input input)))


(defn part-2
  [input]
  (let [preferences (parse-input input)]
    (-> (apply merge-with merge
               preferences
               (map #(hash-map % {"NPE" 0}) (keys preferences)))
        (assoc "NPE" (->> (keys preferences)
                          (map #(vector % 0))
                          (into {})))
        optimal-seating-score)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 330
  (part-1 task-input)                                       ; => 709

  ;; Part 2
  (part-2 test-input)                                       ; => 286
  (part-2 task-input)                                       ; => 668

  )
