(ns aoc-2015.day-12
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 12: JSAbacusFramework.io --- https://adventofcode.com/2015/day/12

(def task-input (u/slurp-resource "inputs/aoc_2015/day-12.txt"))

(defn flatten-all
  "like flatten, but also flattens maps and sets"
  [x]
  (let [target? (u/or-fn sequential? set? map?)]
    (filter (complement target?)
            (rest (tree-seq target? seq x)))))


(defn part-1
  [input]
  (->> (string/replace input \: \space)
       read-string
       flatten-all
       (filter number?)
       (apply +)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 191164

  ;; Part 2
  (part-2 task-input)                                       ; =>

  )
