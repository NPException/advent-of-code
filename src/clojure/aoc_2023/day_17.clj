(ns aoc-2023.day-17
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 17: Clumsy Crucible ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-17.txt"))

(def test-input "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533")
(def test-input-2 "111111111111\n999999999991\n999999999991\n999999999991\n999999999991")


(defn loss-at
  ^long [grid [x y]]
  (- (int (u/nth-in grid [y x]))
     48))

(defn find-heat-loss
  [input build-neighbour-fn build-end-fn]
  (let [grid         (str/split-lines input)
        min-x        0
        max-x        (dec (count (first grid)))
        min-y        0
        max-y        (dec (count grid))
        start        [min-x min-y]
        end          [max-x max-y]
        neighbour-fn (build-neighbour-fn min-x max-x min-y max-y)
        end?         (build-end-fn end)]
    (->> (u/A*-search
           [[start [0 0]]]
           end?
           neighbour-fn
           (fn [_] 0)
           (fn [_ [next-pos]]
             (loss-at grid next-pos)))
         (rest)
         (map #(loss-at grid (first %)))
         (apply +))))


(defn part-1
  [input]
  (find-heat-loss input
    (fn [^long min-x ^long max-x ^long min-y ^long max-y]
      (fn [[[^long x ^long y] [^long dx ^long dy]]]
        (filterv some?
          ; left
          [(when (and (> x min-x) (<= -2 dx 0))
             [[(dec x) y] [(dec dx) 0]])
           ; right
           (when (and (< x max-x) (<= 0 dx 2))
             [[(inc x) y] [(inc dx) 0]])
           ; up
           (when (and (> y min-y) (<= -2 dy 0))
             [[x (dec y)] [0 (dec dy)]])
           ; down
           (when (and (< y max-y) (<= 0 dy 2))
             [[x (inc y)] [0 (inc dy)]])])))
    (fn [end]
      (fn [[pos _dxy]]
        (= pos end)))))



;; PART 2

(defn can-change?
  [^long steps-taken]
  (>= (abs steps-taken) 4))

(defn can-continue?
  [^long d]
  (and (< 0 d) (< d 9)))


(defn part-2
  [input]
  (find-heat-loss input
    ; neighbours fn
    (fn [^long min-x ^long max-x ^long min-y ^long max-y]
      (fn [[[^long x ^long y] [^long dx ^long dy]]]
        (filterv some?
          ; left
          [(when (and (> x min-x)
                      (or (can-change? dy)
                          (can-continue? (- dx))))
             [[(dec x) y] [(dec dx) 0]])
           ; right
           (when (and (< x max-x)
                      (or (and (zero? dx) (zero? dy))
                          (can-change? dy)
                          (can-continue? dx)))
             [[(inc x) y] [(inc dx) 0]])
           ; up
           (when (and (> y min-y)
                      (or (can-change? dx)
                          (can-continue? (- dy))))
             [[x (dec y)] [0 (dec dy)]])
           ; down
           (when (and (< y max-y)
                      (or (and (zero? dx) (zero? dy))
                          (can-change? dx)
                          (can-continue? dy)))
             [[x (inc y)] [0 (inc dy)]])])))
    ; end state check
    (fn [end]
      (fn [[pos [dx dy]]]
        (and (= pos end)
             (or (can-change? dx)
                 (can-change? dy)))))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 102
  (part-1 test-input-2)                                     ; => 59
  (part-1 task-input)                                       ; => 817
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 94
  (part-2 test-input-2)                                     ; => 71
  (part-2 task-input)                                       ; => 945 TOO HIGH - both examples work though... :/
  (crit/quick-bench (part-2 task-input))

  )
