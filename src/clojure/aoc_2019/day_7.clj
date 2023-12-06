(ns aoc-2019.day-7
  (:require [aoc-2019.intcode-interpreter :as intcode]
            [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 7: Amplification Circuit ---

(def task-input (u/read-as-vector (u/slurp-resource "inputs/aoc_2019/day-7.txt")))

(def test-input (u/read-as-vector "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\n1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))


(defn run-with-settings
  [state settings]
  [(reduce
     (fn [prev-amp-output setting]
       (-> (update state :input conj setting prev-amp-output)
           (intcode/run-program)
           :output
           (first)))
     0
     settings)
   settings])


(defn part-1
  [mem]
  (let [state (intcode/create-state mem)]
    (->> (u/permutations [0 1 2 3 4])
         (map #(run-with-settings state %))
         (apply max-key first))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => [65210, [1,0,4,3,2]]
  (part-1 task-input)                                       ; => [225056 [0 1 2 4 3]]
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
