(ns aoc-2022.day-3-jeremy-algo
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 3: Rucksack Reorganization ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-3.txt"))

(def test-input "vJrwpWtwJgWrhcsFMMfFFhFpc\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")

(defn priority
  ^long [^long c]
  (if (>= c 97)
    (- c 96)
    (- c 38)))


(defn as-bits
  ^long [^String s]
  (loop [i (dec (.length s))
         bits 0]
    (if (< i 0)
      bits
      (recur (dec i) (bit-or bits (bit-shift-left 1 (priority (.codePointAt s i))))))))


(defn part-1
  [input]
  (->> (str/split-lines input)
       (transduce
         (map (fn [line]
                (let [len  (count line)
                      half (/ len 2)]
                  (Long/numberOfTrailingZeros
                    (bit-and
                      (as-bits (subs line 0 half))
                      (as-bits (subs line half)))))))
         +
         0)))


(defn part-2
  [input]
  (->> (str/split-lines input)
       (transduce
         (comp
           (map as-bits)
           (u/partitioning 3)
           (map (fn [[^long a ^long b ^long c]]
                  (Long/numberOfTrailingZeros
                    (-> (bit-and a b)
                        (bit-and c))))))
         +
         0)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 157
  (part-1 task-input)                                       ; => 7674
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 70
  (part-2 task-input)                                       ; => 2805
  (crit/quick-bench (part-2 task-input))

  )
