(ns aoc-2022.day-12
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 12: Hill Climbing Algorithm ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-12.txt"))

(def test-input "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")


(defn coordinate-of
  [grid c]
  (->> (map-indexed vector grid)
       (keep (fn [[y row]]
               (when-let [x (str/index-of row c)]
                 [x y])))
       (first)))


(defn parse-input
  [input]
  (let [grid (str/split-lines input)]
    [(->> grid
          (mapv #(-> (str/replace % \S \a)
                     (str/replace \E \z)))
          (mapv #(mapv int %)))
     (coordinate-of grid \S)
     (coordinate-of grid \E)]))


(defn at
  [grid x y]
  (u/nth-in grid [y x] nil))


(defn next-possible-steps
  [grid [x y]]
  (let [min-height (dec (at grid x y))]
    (->> [[0 -1] [0 1] [-1 0] [1 0]]                        ;; direction offsets
         (mapv (fn [[ox oy]]
                 [(+ x ox) (+ y oy)]))
         (filterv (fn [[nx ny]]
                    (some-> (at grid nx ny)
                            (>= min-height)))))))


(defn solve
  [grid start goal-fn]
  (->> (u/A*-search [start]
         goal-fn
         #(next-possible-steps grid %)
         (constantly 0)
         (constantly 1))
       (count)
       (dec)))


(defn part-1
  [input]
  (let [[grid start end] (parse-input input)]
    (solve grid end
      #(= % start))))


(defn part-2
  [input]
  (let [[grid _ end] (parse-input input)
        target-height (int \a)]
    (solve grid end
      (fn [[x y]]
        (= target-height (at grid x y))))))


;; TODO: visualize the path
(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 31
  (part-1 task-input)                                       ; => 517
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 29
  (part-2 task-input)                                       ; => 512
  (crit/quick-bench (part-2 task-input))

  )
