(ns aoc-2015.day-16
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 16: Aunt Sue --- https://adventofcode.com/2015/day/16

(def task-input (->> (u/slurp-resource "inputs/aoc_2015/day-16.txt")
                     (str/split-lines)
                     (mapv #(as-> % $
                                  (str/replace $ \: \space)
                                  (str "{" $ "}")
                                  (read-string $)
                                  (u/keywordize-keys $)))))

(def known-facts {:children 3
                  :cats 7
                  :samoyeds 2
                  :pomeranians 3
                  :akitas 0
                  :vizslas 0
                  :goldfish 5
                  :trees 3
                  :cars 2
                  :perfumes 1})

(defn solve
  [input pred]
  (u/first-match
    #(every? (fn [[k v]] (or (= k :Sue) (pred k v))) %)
    input))

(defn part-1
  [input]
  (solve input (fn [k v] (= v (known-facts k)))))

(defn part-2
  [input]
  (solve input (fn [k v]
                 (cond
                   (#{:cats :trees} k) (> v (known-facts k))
                   (#{:pomeranians :goldfish} k) (< v (known-facts k))
                   :else (= v (known-facts k))))))


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => {:Sue 103, :cars 2, :perfumes 1, :goldfish 5}
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 task-input)                                       ; => {:Sue 405, :trees 8, :perfumes 1, :cars 2}
  (quick-bench (part-2 task-input))

  )
