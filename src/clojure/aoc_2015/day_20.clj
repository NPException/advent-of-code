(ns aoc-2015.day-20
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [criterium.core :as crit]
            [aoc-utils :as u]))

;; --- Day 20: Infinite Elves and Infinite Houses ---

(def task-input (parse-long (u/slurp-resource "inputs/aoc_2015/day-20.txt")))

(def test-input 150)

(defn enough-presents?
  [^long house ^long target-presents ^long stop-number ^long present-factor]
  (let [^long presents (->> (u/find-all-divisors house)
                            (reduce
                              (fn [^long sum ^long elf]
                                (if (> (quot house elf) stop-number)
                                  sum
                                  (+ sum (* elf present-factor))))
                              0))]
    (>= presents target-presents)))


(defn find-lowest-house-number
  [target-presents stop-number present-factor]
  (->> (range 1 Long/MAX_VALUE)
       (u/first-match #(enough-presents? % target-presents stop-number present-factor))))


(defn part-1
  [target-presents]
  (find-lowest-house-number target-presents Long/MAX_VALUE 10))


(defn part-2
  [target-presents]
  (find-lowest-house-number target-presents 50 11))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 8
  (part-1 task-input)                                       ; => 665280
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 task-input)                                       ; => 705600
  (crit/quick-bench (part-2 task-input))

  )
