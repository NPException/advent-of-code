(ns aoc-2019.day-5
  (:require [aoc-2019.intcode-interpreter :as intcode]
            [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 5: Sunny with a Chance of Asteroids ---

(def task-input (u/slurp-resource "inputs/aoc_2019/day-5.txt"))



(defn part-1
  [input]
  (-> (u/read-as-vector input)
      (intcode/create-state)
      (update :input conj 1)
      (intcode/run-program)
      :output))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => [0 0 0 0 0 0 0 0 0 11933517]
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
