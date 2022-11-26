(ns aoc-2015.day-23
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 23: Opening the Turing Lock ---

(def task-input (u/slurp-resource "inputs/aoc_2015/day-23.txt"))

(def test-input "inc a\njio a, +2\ntpl a\ninc a")


(defn inc-ip
  [state]
  (update state :ip inc))

(defn jmp
  [offset state]
  (update state :ip + offset))

(def instructions
  ;hlf r sets register r to half its current value, then continues with the next instruction.
  {'hlf (fn [r state]
          (inc-ip (update state r quot 2)))
   ;tpl r sets register r to triple its current value, then continues with the next instruction.
   'tpl (fn [r state]
          (inc-ip (update state r * 3)))
   ;inc r increments register r, adding 1 to it, then continues with the next instruction.
   'inc (fn [r state]
          (inc-ip (update state r inc)))
   ;jmp offset is a jump; it continues with the instruction offset away relative to itself.
   'jmp jmp
   ;jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
   'jie (fn [r offset state]
          (if (even? (state r))
            (jmp offset state)
            (inc-ip state)))
   ;jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).
   'jio (fn [r offset state]
          (if (= 1 (state r))
            (jmp offset state)
            (inc-ip state)))})


(defn compile-program
  [input]
  (->> (str/split-lines input)
       (mapv (fn [mnemonic]
               (let [[op & args] (u/read-as-vector mnemonic)]
                 (apply partial (instructions op) args))))))


(defn run-instruction
  [program state]
  (when-let [instruction (nth program (:ip state) nil)]
    (instruction state)))

(defn run-program
  [input start-state]
  (let [program (compile-program input)]
    (->> (iterate #(run-instruction program %) start-state)
         (take-while some?)
         (last))))


(defn part-1
  [input target-register]
  (-> (run-program input {:ip 0, 'a 0, 'b 0})
      (target-register)))


(defn part-2
  [input]
  ('b (run-program input {:ip 0, 'a 1, 'b 0})))


(comment
  ;; Part 1
  (part-1 test-input 'a)                                    ; => 2
  (part-1 task-input 'b)                                    ; => 170
  (crit/quick-bench (part-1 task-input 'b))

  ;; Part 2
  (part-2 task-input)                                       ; => 247
  (crit/quick-bench (part-2 task-input))

  )
