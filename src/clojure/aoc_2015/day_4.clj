(ns aoc-2015.day-4
  (:use [criterium.core])
  (:require [clojure.string :as string]
            [aoc-utils :as u])
  (:import [java.security MessageDigest]))

;; --- Day 4: The Ideal Stocking Stuffer --- https://adventofcode.com/2015/day/4

(def task-input
  (u/slurp-resource "inputs/aoc_2015/day-4.txt"))


(defn md5
  [^String s]
  (->> (.getBytes s)
       (.digest (MessageDigest/getInstance "MD5"))
       u/bytes->hex))


(defn hash-fn
  [secret hash-target]
  (fn [n]
    (let [hash (md5 (str secret n))]
      (when (string/starts-with? hash hash-target)
        [n hash]))))


(defn mine-advent-coin
  [secret hash-target]
  (->> (iterate inc 1)
       (u/cpmap 1000 (hash-fn secret hash-target))
       #_(map (hash-fn secret hash-target))                 ;; takes ~6x as long for part 2 compared to u/cpmap
       (filter some?)
       first))


(comment
  ;; Part 1
  (mine-advent-coin task-input "00000")                     ;; => 282749
  (quick-bench (mine-advent-coin task-input "00000"))
  ;; Part 2
  (mine-advent-coin task-input "000000")                    ;; => 9962624
  (quick-bench (mine-advent-coin task-input "000000"))
  )
