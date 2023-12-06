(ns aoc-2019.day-7
  (:require [aoc-2019.intcode-interpreter :as ic]
            [aoc-utils :as u]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 7: Amplification Circuit ---

(def task-input (u/read-as-vector (u/slurp-resource "inputs/aoc_2019/day-7.txt")))

(def test-input (u/read-as-vector "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\n1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
(def test-input-2 (u/read-as-vector "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\n27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))


(defn then-run
  [prev-amp amp]
  (-> (update amp :input conj (ic/last-output prev-amp))
      (ic/run-program)))

(defn run-with-settings
  [state settings]
  (let [[a b c d e] (mapv #(ic/push-input state %) settings)
        e (ic/push-output e 0)]
    [(loop [[a b c d e :as amps] [a b c d e]]
       (if (some :halt? amps)
         (ic/last-output e)
         (let [a (then-run e a)
               b (then-run a b)
               c (then-run b c)
               d (then-run c d)
               e (then-run d e)]
           (recur [a b c d e]))))
     settings]))


(defn part-1
  [mem]
  (let [state (ic/create-state mem)]
    (->> (u/permutations [0 1 2 3 4])
         (map #(run-with-settings state %))
         (apply max-key first))))


(defn part-2
  [mem]
  (let [state (ic/create-state mem)]
    (->> (u/permutations [5 6 7 8 9])
         (map #(run-with-settings state %))
         (apply max-key first))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => [65210 [1 0 4 3 2]]
  (part-1 task-input)                                       ; => [225056 [0 1 2 4 3]]
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-2)                                     ; => [139629729 [9 8 7 6 5]]
  (part-2 task-input)                                       ; => [14260332 [8 5 9 6 7]]
  (crit/quick-bench (part-2 task-input))

  )
