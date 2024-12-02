(ns aoc-2024.day-2
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

;; --- Day 2: Red-Nosed Reports ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-2.txt"))

(def test-input "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9")

(defn parse-input
  [text]
  (->> (str/split-lines text)
       (mapv u/read-as-vector)))


(defn proper-slope?
  [report]
  (boolean (or (apply < report)
               (apply > report))))

(defn proper-difference?
  [report]
  (->> (u/vpartition 2 1 report)
       (every? (fn [[a b]]
                 (<= 1 (abs (- a b)) 3)))))

(def safe-report? (u/and-fn proper-slope? proper-difference?))


(defn part-1
  [input]
  (->> (parse-input input)
       (u/count-matching safe-report?)))


(defn safe-dampened-report?
  [report]
  (or (safe-report? report)
      (some safe-report?
        (for [i (range 0 (count report))]
          (into (subvec report 0 i) (subvec report (inc i)))))))

(defn part-2
  [input]
  (->> (parse-input input)
       (u/count-matching safe-dampened-report?)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 2
  (part-1 task-input)                                       ; => 516
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 4
  (part-2 task-input)                                       ; => 561
  (crit/quick-bench (part-2 task-input))

  )
