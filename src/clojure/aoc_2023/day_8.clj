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
(def test-input-2 "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)")


(defn parse-input
  [input]
  (let [[directions node-lines] (str/split input #"\n\n")]
    [directions
     (->> (str/split-lines node-lines)
          (map (fn [line]
                 (let [[key left right] (str/split line #" = \(|, |\)")]
                   [key {\L left, \R right}])))
          (into {}))]))


(defn solve
  [directions lookup start end?]
  (reduce
    (fn [[node steps] direction]
      (if (end? node)
        (reduced steps)
        [(get-in lookup [node direction])
         (inc steps)]))
    [start 0]
    (cycle directions)))


(defn part-1
  [input]
  (let [[directions lookup] (parse-input input)]
    (solve directions lookup "AAA" #(= % "ZZZ"))))


(defn part-2
  [input]
  (let [[directions lookup] (parse-input input)
        start-nodes (filterv #(str/ends-with? % "A") (keys lookup))
        end?        #(str/ends-with? % "Z")
        node-steps  (mapv #(solve directions lookup % end?) start-nodes)]
    (apply u/least-common-multiple node-steps)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 6
  (part-1 task-input)                                       ; => 11911
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-2)                                     ; => 6
  (part-2 task-input)                                       ; => 10151663816849
  (crit/quick-bench (part-2 task-input))

  )
