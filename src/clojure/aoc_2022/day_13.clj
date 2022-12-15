(ns aoc-2022.day-13
  (:require [aoc-utils :as u]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 13: Distress Signal ---

(def task-input (u/read-as-vector (u/slurp-resource "inputs/aoc_2022/day-13.txt")))

(def test-input (u/read-as-vector "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"))


(defn compare-order
  [left right]
  (if (and (int? left) (int? right))
    (Long/compare left right)
    (let [left  (if (vector? left) left [left])
          right (if (vector? right) right [right])]
      (or (u/first-match #(not= 0 %) (mapv compare-order left right))
          (Long/compare (count left) (count right))))))


(defn part-1
  [input]
  (->> (partition 2 input)
       (keep-indexed (fn [i [left right]]
                       (when (== -1 (compare-order left right))
                         (inc i))))
       (apply +)))


(defn part-2
  [input]
  (let [dividers #{[[2]]
                   [[6]]}]
    (->> (into input dividers)
         (sort compare-order)
         (keep-indexed #(when (dividers %2) (inc %1)))
         (apply *))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 13
  (part-1 task-input)                                       ; => 6272
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 140
  (part-2 task-input)                                       ; => 22288
  (crit/quick-bench (part-2 task-input))

  )
