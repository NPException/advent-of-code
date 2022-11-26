(ns aoc-2015.day-24
  (:require [aoc-utils :as u]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 24: It Hangs in the Balance ---

(def task-input (u/read-as-vector (u/slurp-resource "inputs/aoc_2015/day-24.txt")))

(def test-input [1 2 3 4 5 7 8 9 10 11])


(defn quantum-entanglement
  [group]
  (apply * group))


(defn target-size
  [packages nr-of-groups]
  (/ (apply + packages) nr-of-groups))


(defn min-nr-elements
  [sum elements]
  (let [sorted (sort > elements)]
    (->> (range 1 (inc (count elements)))
         (filter #(<= sum (apply + (take % sorted))))
         first)))


(defn viable-combinations
  [target min-nr packages]
  (loop [nr min-nr]
    (if-let [viable (->> (u/combinations nr packages)
                         (filter #(= target (apply + %)))
                         seq)]
      viable
      (recur (inc nr)))))


(defn find-lowest-quantum-entanglement
  [packages nr-of-groups]
  (let [target (target-size packages nr-of-groups)
        nr     (min-nr-elements target packages)]
    (->> (viable-combinations target nr packages)
         (map quantum-entanglement)
         (sort)
         first)))


(defn part-1
  [packages]
  (find-lowest-quantum-entanglement packages 3))


(defn part-2
  [packages]
  (find-lowest-quantum-entanglement packages 4))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 99
  (part-1 task-input)                                       ; => 10439961859
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 44
  (part-2 task-input)                                       ; => 72050269
  (crit/quick-bench (part-2 task-input))

  )
