(ns aoc-2015.day-12
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 12: JSAbacusFramework.io --- https://adventofcode.com/2015/day/12

(def task-input (u/slurp-resource "inputs/aoc_2015/day-12.txt"))

(defn flatten-with
  "flattens all nested structures that return true for the passed predicate"
  [target? x]
  (filter (complement target?)
          (rest (tree-seq target? seq x))))


(defn sum
  [input target?]
  (->> (string/replace input \: \space)
       read-string
       (flatten-with target?)
       (filter number?)
       (apply +)))


(defn part-1
  [input]
  (sum input (u/or-fn sequential? set? map?)))


(defn part-2
  [input]
  (sum input
       (u/and-fn
         (u/or-fn sequential? set? map?)
         #(not (and (map? %)
                    (some #{"red"} (vals %)))))))


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 191164

  ;; Part 2
  (part-2 task-input)                                       ; => 87842

  )
