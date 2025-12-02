(ns aoc-2025.day-1
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 1: Secret Entrance ---

(def task-input (u/slurp-resource "inputs/aoc_2025/day-1.txt"))

(def test-input "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82")


(defn parse-input
  [input]
  (-> (str/replace input \L \-)
      (str/replace \R \+)
      (u/read-as-vector)))


(defn part-1
  [input]
  (->> (parse-input input)
       (reduce (fn [[n dial] turn]
                 (let [dial (mod (+ dial turn) 100)]
                   (if (zero? dial)
                     [(inc n) dial]
                     [n dial])))
         [0 50])
       (first)))


(defn part-2
  [input]
  (->> (parse-input input)
       (reduce (fn [[n dial] turn]
                 (let [starts-zero? (zero? dial)
                       dial (+ dial turn)
                       bonus (if (and (not starts-zero?)
                                      (<= dial 0))
                               1
                               0)]
                   [(+ n (abs (quot dial 100)) bonus) (mod dial 100)]))
         [0 50])
       (first)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 3
  (part-1 task-input)                                       ; => 1150
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 6
  (part-2 task-input)                                       ; => 6738
  (crit/quick-bench (part-2 task-input))

  )
