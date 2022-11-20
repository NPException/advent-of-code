(ns aoc-2015.day-19
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [criterium.core :as crit]
            [aoc-utils :as u]))

;; --- Day 19: Medicine for Rudolph --- https://adventofcode.com/2015/day/19

(def task-input (u/slurp-resource "inputs/aoc_2015/day-19.txt"))

(def test-input-1 "e => H\ne => O\nH => HO\nH => OH\nO => HH\n\nHOH")
(def test-input-2 "e => H\ne => O\nH => HO\nH => OH\nO => HH\n\nHOHOHO")


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


; NOTE: Rn = (
;       Ar = )
;        Y = ,
; I did not manage to find a solution other than brute force myself,
; and was too lazy to let it finish running.
; The current solution is based on this explanation on reddit: https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/

(defn part-2
  [input]
  (let [[_ molecule] (parse-input input)
        elements (re-seq #"[A-Z][a-z]?" molecule)]
    ; count(tokens) - count("(" or ")") - 2*count(",") - 1
    (- (count elements)
       (u/count-matching #{"Rn" "Ar"} elements)
       (* 2 (u/count-matching #{"Y"} elements))
       1)))


(comment
  ;; Part 1
  (part-1 test-input-1)                                     ; => 4
  (part-1 test-input-2)                                     ; => 7
  (part-1 task-input)                                       ; => 509
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-1)                                     ; => 2
  (part-2 test-input-2)                                     ; => 5
  (part-2 task-input)                                       ; => 195
  (crit/quick-bench (part-2 task-input))

  )
