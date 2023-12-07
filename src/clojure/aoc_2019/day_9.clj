(ns aoc-2019.day-9
  (:require [aoc-2019.intcode-interpreter :as ic]
            [aoc-utils :as u]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 9: Sensor Boost ---

(def boost-program (u/read-as-vector (u/slurp-resource "inputs/aoc_2019/day-9.txt")))


(defn run-boost-program
  [mode]
  (-> (ic/create-state boost-program)
      (ic/push-input mode)
      (ic/run-program)
      :output))


(comment
  ;; Part 1
  (run-boost-program 1)                                     ; => [2427443564]

  ;; Part 2
  (run-boost-program 2)                                     ; => [87221]

  )
