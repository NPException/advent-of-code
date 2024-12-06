(ns aoc-2024.day-6
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 6: Guard Gallivant ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-6.txt"))

(def test-input "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#...")

(def up 0)
(def right 1)
(def down 2)
(def left 3)

;; directions up, right, down, left
(def offsets
  [[0 -1]
   [1 0]
   [0 1]
   [-1 0]])


(defn parse-grid
  [input]
  (let [grid (str/split-lines input)
        [x y] (first (u/find-grid-elements grid (fn [[_ _ v]] (= v \^))))]
    [grid x y]))


(defn part-1
  [input]
  (let [[grid gx gy] (parse-grid input)]
    (loop [^long direction up
           ^long x gx
           ^long y gy
           seen #{[gx gy \^]}]
      (let [[^long dx ^long dy] (nth offsets direction)
            nx (+ x dx)
            ny (+ y dy)
            [_ _ value :as tile] (u/grid-element grid nx ny nil)]
        (cond
          ; end of path
          (nil? value)
          (count seen)
          ; hit a wall
          (= value \#)
          (recur (-> (inc direction) (mod 4)) x y seen)
          ; move forward
          :else
          (recur direction nx ny (conj seen tile)))))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 41
  (part-1 task-input)                                       ; => 4752
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
