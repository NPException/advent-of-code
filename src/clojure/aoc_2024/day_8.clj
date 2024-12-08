(ns aoc-2024.day-8
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 8: Resonant Collinearity ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-8.txt"))

(def test-input "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............")


(defn print-with-antinodes
  [grid antinodes]
  (->> antinodes
       (reduce
         (fn [acc [x y element]]
           (update-in acc [y x] #(if (= % \.) element %)))
         grid)
       (mapv #(apply str %) grid)
       (str/join \newline)
       (println)))

(defn parse-grid
  [text]
  (->> (str/split-lines text)
       (mapv vec)))


(defn find-antennas
  [grid]
  (->> (u/grid-elements grid)
       (remove (fn [[_ _ element]] (= element \.)))
       (group-by peek)))


(defn antinode-pair
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    [[(- x1 dx) (- y1 dy) \#]
     [(+ x2 dx) (+ y2 dy) \#]]))

(defn find-antinodes
  [antennas]
  (let [length (count antennas)]
    (apply concat
      (for [i (range 0 (dec length))]
        (->> (range (inc i) length)
             (mapcat #(antinode-pair (nth antennas i) (nth antennas %))))))))


(defn in-bounds?
  [width height [x y]]
  (and (<= 0 x) (< x width)
       (<= 0 y) (< y height)))


(defn part-1
  [input]
  (let [grid (parse-grid input)
        width (count (first grid))
        height (count grid)
        antennas (find-antennas grid)
        antinodes (->> (vals antennas)
                       (mapcat find-antinodes)
                       (set)
                       (filterv #(in-bounds? width height %)))]
    #_(print-with-antinodes grid antinodes)
    (count antinodes)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 14
  (part-1 task-input)                                       ; =>
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )

;......#....#
;...#....0...
;....#0....#.
;..#....0....
;....0....#..
;.#....A.....
;...#........
;#......#....
;........A...
;.........A..
;..........#.
;..........#.

;......#....#
;...#....0...
;....#0....#.
;..#....0....
;....0....#..
;.#....#.....
;...#........
;#......#....
;........A...
;.........A..
;..........#.
;..........#.
