(ns aoc-2023.day-10
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 10: Pipe Maze ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-10.txt"))

(def test-input "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ")

;| is a vertical pipe connecting north and south.
;- is a horizontal pipe connecting east and west.
;L is a 90-degree bend connecting north and east.
;J is a 90-degree bend connecting north and west.
;7 is a 90-degree bend connecting south and west.
;F is a 90-degree bend connecting south and east.
;. is ground; there is no pipe in this tile.

(defn replace-pipes
  [input]
  (str/replace input #"[|\-LJ7F.]"
    {"|" "┃"
     "-" "━"
     "L" "┗"
     "J" "┛"
     "7" "┓"
     "F" "┏"
     "." " "}))


(defn parse-input
  [input]
  (str/split-lines (replace-pipes input)))

(defn neighbours
  [[^long x ^long y pipe]]
  (case pipe
    \┃ [[x (dec y)] [x (inc y)]]
    \━ [[(dec x) y] [(inc x) y]]
    \┗ [[x (dec y)] [(inc x) y]]
    \┛ [[(dec x) y] [x (dec y)]]
    \┓ [[(dec x) y] [x (inc y)]]
    \┏ [[(inc x) y] [x (inc y)]]))

(defn next-pipe
  [grid [prev-x prev-y] node]
  (let [[[nx1 ny1] [nx2 ny2]] (neighbours node)]
    (if (and (= prev-x nx1)
             (= prev-y ny1))
      (u/grid-element grid nx2 ny2)
      (u/grid-element grid nx1 ny1))))


(defn connected?
  [[src-x src-y] [_ _ pipe :as node]]
  (and (#{\┃ \━ \┗ \┛ \┓ \┏} pipe)
       (let [[[nx1 ny1] [nx2 ny2]] (neighbours node)]
         (or (and (= src-x nx1) (= src-y ny1))
             (and (= src-x nx2) (= src-y ny2))))))

(defn first-pipe
  [grid [x y :as start]]
  (->> [(u/grid-element grid (dec x) y nil)
        (u/grid-element grid x (dec y) nil)
        (u/grid-element grid (inc x) y nil)
        (u/grid-element grid x (inc y) nil)]
       (filterv #(some? (nth % 2)))
       (u/first-match #(connected? start %))))

(defn find-path
  [grid]
  (let [start      (->> (u/grid-elements grid)
                        (u/first-match #(= \S (nth % 2))))
        first-node (first-pipe grid start)]
    (->> [start first-node]
         (iterate (fn [[prev current]]
                    [current (next-pipe grid prev current)]))
         (take-while
           (fn [[_ node]]
             (not= node start)))
         (mapv second)
         (cons start)
         vec)))


(defn part-1
  [input]
  (let [grid       (parse-input input)]
    (-> (find-path grid)
        (count)
        (/ 2))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 8
  (part-1 task-input)                                       ; => 6838
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
