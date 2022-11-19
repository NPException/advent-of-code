(ns aoc-2015.day-19
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [criterium.core :as crit]
            [aoc-utils :as u]))

;; --- Day 19: Medicine for Rudolph --- https://adventofcode.com/2015/day/19

(def task-input (u/slurp-resource "inputs/aoc_2015/day-19.txt"))

(def test-input-1 "H => HO\nH => OH\nO => HH\n\nHOH")
(def test-input-2 "H => HO\nH => OH\nO => HH\n\nHOHOHO")


(defn parse-input
  [input]
  (let [[replacement-lines
         [_ molecule]] (->> (str/split-lines input)
                            (split-with seq))]
    [(mapv #(str/split % #" => ") replacement-lines)
     molecule]))


(defn replace-every
  [^String src ^String target ^String replacement]
  (iteration
    (fn [^long start-index]
      (let [found-index (.indexOf src target start-index)]
        (when-not (neg? found-index)
          [(inc found-index)
           (str (subs src 0 found-index)
             replacement
             (subs src (+ found-index (.length target)) (.length src)))])))
    :initk 0
    :kf #(nth % 0)
    :vf #(nth % 1)))


(defn part-1
  [input]
  (let [[replacements molecule] (parse-input input)]
    (->> replacements
         (mapcat #(apply replace-every molecule %))
         set
         count)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input-1)                                     ; => 4
  (part-1 test-input-2)                                     ; => 7
  (part-1 task-input)                                       ; => 509
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-1)                                     ; =>
  (part-2 test-input-2)                                     ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
