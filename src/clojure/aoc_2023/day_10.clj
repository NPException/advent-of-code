(ns aoc-2023.day-10
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 10: Pipe Maze ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-10.txt"))

(def test-input "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ")

(def test-input-2 "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L")

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
  (let [grid (parse-input input)]
    (-> (find-path grid)
        (count)
        (/ 2))))


(defn direction
  [[x y] [nx ny]]
  (case [(- nx x) (- ny y)]
    [0 -1] :up
    [1 0] :right
    [0 1] :down
    [-1 0] :left))


; replaces the "S" with it's equivalent pipe character
(defn replace-start
  [[[x y :as start] & more :as path]]
  (let [replacement (case #{(direction start (peek path))
                            (direction start (first more))}
                      #{:up :down} \┃
                      #{:left :right} \━
                      #{:up :right} \┗
                      #{:up :left} \┛
                      #{:down :left} \┓
                      #{:down :right} \┏)]
    (apply vector [x y replacement] more)))

; ┏━━━━━━━┓
; ┃   ┏━┓ ┃
; ┃I┏━┛ ┃ ┃
; ┗━┛   ┗━┛
; traces towards the right of the grid, and counts how often the path is crossed
(defn count-intersections
  [width on-path start-x y]
  (loop [n              0
         x              start-x
         crossing-start nil]
    (if (= x width)
      n
      (if-let [pipe (on-path [x y])]
        (case pipe
          ; up/down intersection
          \┃ (recur (inc n) (inc x) nil)
          ; tangent
          \━ (recur n (inc x) crossing-start)
          ; entry corners
          \┗ (recur n (inc x) :up)
          \┏ (recur n (inc x) :down)
          ; exit corners
          \┛ (recur
               (if (= crossing-start :up) n (inc n))
               (inc x)
               nil)
          \┓ (recur
               (if (= crossing-start :down) n (inc n))
               (inc x)
               nil))
        ; not on path
        (recur n (inc x) nil)))))


(defn inside-path?
  [width on-path [x y]]
  ; must not lie directly on path
  (when-not (on-path [x y])
    ; an odd number of polygon intersections means that the point is inside
    (odd? (count-intersections width on-path (inc x) y))))


(defn part-2
  [input]
  (let [grid     (parse-input input)
        width    (count (first grid))
        elements (u/grid-elements grid)
        path     (replace-start (find-path grid))
        ; map of coordinate -> pipe on path
        on-path  (->> path
                      (map (fn [[x y e]]
                             [[x y] e]))
                      (into {}))]
    (->> elements
         (filter #(inside-path? width on-path %))
         (count))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 8
  (part-1 task-input)                                       ; => 6838
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-2)                                     ; => 10
  (part-2 task-input)                                       ; => 451
  (crit/quick-bench (part-2 task-input))

  )
