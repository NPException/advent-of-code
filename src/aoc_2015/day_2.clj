(ns aoc-2015.day-2
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 2: I Was Told There Would Be No Math --- https://adventofcode.com/2015/day/2

(def task-input
  (u/slurp-resource "inputs/aoc_2015/day-2.txt"))

(def sizes
  (->> (string/split-lines task-input)
       (map #(string/split % #"x"))
       (mapv #(mapv u/parse-int %))))

;; part 1
(defn extra-area
  [l w h]
  (min (* l w) (* w h) (* h l)))

(defn box-area
  [l w h]
  (+ (* 2 l w)
     (* 2 w h)
     (* 2 h l)))

(defn paper-needed
  [[l w h]]
  (+ (extra-area l w h)
     (box-area l w h)))


;; part 2
(defn smallest-perimeter
  [l w h]
  (min (+ l l w w)
       (+ w w h h)
       (+ h h l l)))

(defn ribbon-needed
  [[l w h]]
  (+ (smallest-perimeter l w h)
     (* l w h)))



(comment
  ;; Part 1
  (apply + (map paper-needed sizes))
  ;; Part 2
  (apply + (map ribbon-needed sizes))
  )
