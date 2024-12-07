(ns aoc-2024.day-7
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 7: Bridge Repair ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-7.txt"))

(def test-input "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20")

(defn parse-input
  [input]
  (->> (str/replace input ":" "")
       (str/split-lines)
       (mapv u/read-as-vector)))


(defn ||
  "combines the digits from its left and right inputs into a single number"
  [a b]
  (parse-long (str a b)))


(defn valid-number?
  ([ops [goal & nums]]
   (valid-number? ops goal (first nums) (next nums)))
  ([ops goal sum [next-num & more-nums]]
   ; no more numbers left, compare with goal
   (if (nil? next-num)
     (= goal sum)
     ; more numbers left, check if already overshot
     (if (> sum goal)
       false
       ; branch out
       (->> ops (some #(valid-number? ops goal (% sum next-num) more-nums)))))))


(defn solve
  [input ops]
  (->> (parse-input input)
       (filter #(valid-number? ops %))
       (map first)
       (apply +)))


(defn part-1
  [input]
  (solve input [* +]))


(defn part-2
  [input]
  (solve input [|| * +]))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 3749
  (part-1 task-input)                                       ; => 10741443549536
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 11387
  (part-2 task-input)                                       ; => 500335179214836
  (crit/quick-bench (part-2 task-input))

  )
