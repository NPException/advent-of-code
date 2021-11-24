(ns aoc-2020.day-9
  (:require [clojure.string :as string]
            [aoc-utils :as u])
  (:import (clojure.lang IPersistentVector)))

;; --- Day 9: Encoding Error --- https://adventofcode.com/2020/day/9

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-9.txt"))

(def input-numbers (->> task-input
                        string/split-lines
                        (mapv parse-long)))

(defn vpartition
  "Returns a lazy sequence of vectors of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap."
  ;; TODO: implement 4-arity variant with pad collection
  ([n ^IPersistentVector v]
   (vpartition n n v))
  ([n step ^IPersistentVector v]
   (lazy-seq
     (let [num (count v)]
       (when (>= num n)
         (cons (subvec v 0 n)
               (vpartition n step (subvec v step num))))))))


;; part 1 functions

(defn invalid?
  [[n preamble]]
  (empty?
    (for [x preamble
          y preamble
          :when (and (not= x y)
                     (= n (+ x y)))]
      :dummy)))


(defn validation-groups
  [numbers preamble-size]
  (->> (vpartition (inc preamble-size) 1 numbers)
       (map (juxt peek pop))))


;; part 2 functions

(defn find-sum-group
  [numbers target]
  (loop [remaining (drop 2 numbers)
         window (vec (take 2 numbers))]
    (let [sum (apply + window)]
      (cond
        (= sum target) window
        (< sum target) (recur (next remaining)
                              (conj window (first remaining)))
        (> sum target) (recur remaining
                              (vec (rest window)))))))



(comment
  ;; Part 1 => 32321523
  (->> (validation-groups input-numbers 25)
       (filter invalid?)
       ffirst)
  ;; Part 2 => 4794981
  (->> (find-sum-group input-numbers 32321523)
       (apply (juxt min max))
       (apply +))
  )
