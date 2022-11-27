(ns aoc-2016.day-2
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 2: Bathroom Security ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-2.txt"))

(def test-input "ULL\nRRDDD\nLURDL\nUUUUD")


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv str/trim)))


(defn button
  [keypad x y]
  (u/nth-in keypad [y x] nil))


(defn verify
  [keypad x y]
  (when (button keypad x y)
    [x y]))

(defn update-fn
  [xfn yfn]
  (fn [keypad [x y]]
    (or (verify keypad (xfn x) (yfn y))
        [x y])))

(def directions
  {\U (update-fn identity dec)
   \D (update-fn identity inc)
   \L (update-fn dec identity)
   \R (update-fn inc identity)})


(defn next-key
  [keypad [start-key instructions]]
  (when instructions
    [(reduce
       (fn [k inst]
         ((directions inst) keypad k))
       start-key
       (first instructions))
     (next instructions)]))


(defn compute-code
  [input keypad start-key]
  (->> (iterate #(next-key keypad %)
         [start-key (parse-input input)])
       (take-while some?)
       (map first)
       (drop 1)
       (map (fn [[x y]]
              (u/nth-in keypad [y x])))
       (apply str)))


(defn part-1
  [input]
  (compute-code
    input
    [[1 2 3]
     [4 5 6]
     [7 8 9]]
    [1 1]))


(defn part-2
  [input]
  (compute-code
    input
    '[[nil nil   1 nil nil]
      [nil   2   3   4 nil]
      [  5   6   7   8   9]
      [nil   A   B   C nil]
      [nil nil   D nil nil]]
    [0 2]))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1985
  (part-1 task-input)                                       ; => 74921
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 5DB3
  (part-2 task-input)                                       ; => A6B35
  (crit/quick-bench (part-2 task-input))

  )
