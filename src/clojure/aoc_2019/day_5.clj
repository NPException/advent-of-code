(ns aoc-2019.day-5
  (:require [aoc-2019.intcode-interpreter :as intcode]
            [aoc-utils :as u]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 5: Sunny with a Chance of Asteroids ---

(def initial-memory (u/read-as-vector (u/slurp-resource "inputs/aoc_2019/day-5.txt")))


(defn run-diagnostic
  [system-id]
  (-> (intcode/create-state initial-memory)
      (update :input conj system-id)
      (intcode/run-program)
      :output
      last))


(comment
  ;; Part 1
  (run-diagnostic 1)                                        ; => 11933517
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (run-diagnostic 5)                                        ; => 10428568
  (crit/quick-bench (part-2 task-input))

  )
