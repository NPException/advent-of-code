(ns aoc-2025.day-2
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 2: Gift Shop ---

(def task-input (u/slurp-resource "inputs/aoc_2025/day-2.txt"))

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")


(defn parse-input
  [input]
  (->> (str/replace input \- \,)
       (u/read-as-vector)
       (u/vpartition 2)))


(defn twice-repeat-digits?
  [^long n]
  (let [digits (u/num-digits n)]
    (and (even? digits)
         (let [pow-10 (math/pow 10 (quot digits 2))]
           (= (quot n pow-10)
             (mod n pow-10))))))


(defn part-1
  [input]
  (->> (parse-input input)
       (mapcat (fn [[from to]]
                 (range from (inc to))))
       (filterv twice-repeat-digits?)
       (apply +)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1227775554
  (part-1 task-input)                                       ; => 18595663903
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 4174379265
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
