(ns aoc-2023.day-6
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 6: Wait For It ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-6.txt"))

(def test-input "Time:      7  15   30\nDistance:  9  40  200")

(defn parse-line
  [line]
  (->> (str/split line #":")
       (second)
       (u/read-as-vector)))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv parse-line)))


(defn calculate-distance
  [total-time press-time]
  (* press-time (- total-time press-time)))

(defn count-winning-strats
  [time distance]
  (->> (range 1 time)
       (mapv #(calculate-distance time %))
       (u/count-matching #(> % distance))))


(defn part-1
  [input]
  (let [[times distances] (parse-input input)]
    (apply * (map count-winning-strats times distances))))


(defn part-2
  [input]
  (let [[times distances] (parse-input input)]
    (count-winning-strats
      (parse-long (apply str times))
      (parse-long (apply str distances)))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 288
  (part-1 task-input)                                       ; => 800280
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 71503
  (part-2 task-input)                                       ; => 45128024
  (crit/quick-bench (part-2 task-input))

  )
