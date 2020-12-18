(ns aoc-2020.day-18
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 18: Operation Order --- https://adventofcode.com/2020/day/18

(def task-input (u/slurp-resource "inputs/aoc_2020/day-18.txt"))


(defn infix->prefix
  [prio [a op b & [next-op :as remaining]]]
  (cond
    ;; only three elements
    (nil? remaining)
    (list op
          (if (list? a) (infix->prefix prio a) a)
          (if (list? b) (infix->prefix prio b) b))

    ;; first operator takes precedence
    (>= (prio op) (prio next-op))
    (recur prio (cons (list a op b) remaining))

    ;; second operator takes precedence
    :else
    (recur prio [a op (conj remaining b)])))


(defn solve
  [input precedences]
  (->> (string/split-lines input)
       (map #(str "(" % ")"))
       (map read-string)
       (map (partial infix->prefix precedences))
       (map eval)
       (apply +)))


(defn part-1
  [input]
  (solve input {'+ 1 '* 1}))


(defn part-2
  [input]
  (solve input {'+ 2 '* 1}))


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 5783053349377

  ;; Part 2
  (part-2 task-input)                                       ; => 74821486966872

  )
