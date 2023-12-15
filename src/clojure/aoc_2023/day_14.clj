(ns aoc-2023.day-14
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 14: Parabolic Reflector Dish ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-14.txt"))

(def test-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")


(defn column-load
  [grid x]
  (let [height (count grid)]
    (loop [sum 0
           y 0
           max-load height]
      (if (= y height)
        sum
        (case (u/nth-in grid [y x])
          \. (recur sum (inc y) max-load)
          \O (recur (+ sum max-load) (inc y) (dec max-load))
          \# (recur sum (inc y) (dec (- height y))))))))


(defn part-1
  [input]
  (let [grid (str/split-lines input)]
    (->> (range (count (first grid)))
         (mapv #(column-load grid %))
         (apply +))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 136
  (part-1 task-input)                                       ; => 109098
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
