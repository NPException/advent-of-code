(ns aoc-2021.day-20
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 20: Trench Map --- https://adventofcode.com/2021/day/20

(def task-input (u/slurp-resource "inputs/aoc_2021/day-20.txt"))
(def test-input "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###")

(defn parse-input
  [input]
  (let [[algo _ & grid-lines] (str/split-lines input)
        height (count grid-lines)
        width (count (first grid-lines))]
    [(boolean-array (mapv {\# true, \. false} algo))
     ;; bounds is the corner coordinates up to which we need to check on enhancement.
     {:bounds [0 0 (dec width) (dec height)]
      :steps 0
      :active (->> grid-lines
                   (map-indexed
                     (fn [y line]
                       (->> line (keep-indexed
                                   (fn [x c] (when (= c \#) [x y]))))))
                   (apply concat)
                   (into #{}))}]))

(defn activate?
  [x y [^long left ^long top ^long right ^long bottom] active algo steps]
  (let [fill-empty-space? (aget ^booleans algo 0)
        default (and fill-empty-space? (= 1 (rem ^long steps 2)))
        active? (fn [[^long ox ^long oy]]
                  (let [x (+ ^long x ox)
                        y (+ ^long y oy)]
                    ;; bounds used here is the area of known values
                    (if (and (<= left x) (<= x right)
                             (<= top y) (<= y bottom))
                      (active [x y])
                      default)))
        num (loop [[offset & more] '([-1 -1] [0 -1] [1 -1]
                                     [-1 0] [0 0] [1 0]
                                     [-1 1] [0 1] [1 1])
                   i 8, n 0]
              (if (< i 0)
                n
                (recur more (dec i) (if (active? offset)
                                      (bit-or n (bit-shift-left 1 i))
                                      n))))]
    (aget ^booleans algo num)))


(defn simulate
  [algo {:keys [bounds ^long steps active]}]
  (let [[^long l ^long t ^long r ^long b] bounds
        left (dec l), top (dec t)
        right (inc r), bottom (inc b)
        new-active (->> (for [x (range left (inc right))
                              y (range top (inc bottom))]
                          [x y])
                        (u/cpmap 500 (fn [[x y :as coord]]
                                       (when (activate? x y bounds active algo steps)
                                         coord)))
                        (into #{} (filter some?)))]
    {:bounds [left top right bottom]
     :steps (inc steps)
     :active new-active}))


(defn enhance!!!
  [input times]
  (let [[algo field] (parse-input input)]
    (->> (iterate #(simulate algo %) field)
         (drop times)
         first
         :active
         count)))


(def part-1 #(enhance!!! % 2))
(def part-2 #(enhance!!! % 50))

(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 35
  (part-1 task-input)                                       ; => 5231
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 3351
  (part-2 task-input)                                       ; => 14279
  (quick-bench (part-2 task-input))

  )
