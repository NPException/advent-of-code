(ns aoc-2023.day-8
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 8: Haunted Wasteland ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-8.txt"))

(def test-input "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)")

(defn parse-input
  [input]
  (let [[directions node-lines] (str/split input #"\n\n")]
    [directions
     (->> (str/split-lines node-lines)
          (map (fn [line]
                 (let [[key left right] (str/split line #" = \(|, |\)")]
                   [key {\L left, \R right}])))
          (into {}))]))


(defn part-1
  [input]
  (let [[directions lookup] (parse-input input)]
    (reduce
      (fn [[node steps] direction]
        (if (= node "ZZZ")
          (reduced steps)
          [(get-in lookup [node direction])
           (inc steps)]))
      ["AAA" 0]
      (cycle directions))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 6
  (part-1 task-input)                                       ; => 11911
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
