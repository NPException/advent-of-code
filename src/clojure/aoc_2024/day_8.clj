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
       (mapv #(apply str %))
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


(defn in-bounds?
  [width height [x y]]
  (and (<= 0 x) (< x width)
       (<= 0 y) (< y height)))


(defn solve
  [input find-antinodes-fn]
  (let [grid (parse-grid input)
        width (count (first grid))
        height (count grid)
        antennas (find-antennas grid)
        antinodes (->> (vals antennas)
                       (mapcat #(find-antinodes-fn width height %))
                       (set))]
    #_(print-with-antinodes grid antinodes)
    (count antinodes)))


;; PART 1 ;;

(defn antinode-pair
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    [[(- x1 dx) (- y1 dy) \#]
     [(+ x2 dx) (+ y2 dy) \#]]))

(defn find-antinodes-part-1
  [width height antennas]
  (let [length (count antennas)]
    (apply concat
      (for [i (range 0 (dec length))]
        (->> (range (inc i) length)
             (mapcat #(antinode-pair (nth antennas i) (nth antennas %)))
             (filterv #(in-bounds? width height %)))))))

(defn part-1
  [input]
  (solve input find-antinodes-part-1))


;; PART 2 ;;

(defn antinodes-on-line
  [width height [[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        left (->> [x1 y1 \#]
                  (iterate (fn [[x y]]
                             [(- x dx) (- y dy) \#]))
                  (take-while #(in-bounds? width height %)))
        right (->> [x2 y2 \#]
                   (iterate (fn [[x y]]
                              [(+ x dx) (+ y dy) \#]))
                   (take-while #(in-bounds? width height %)))]
    (concat left right)))


(defn find-antinodes-part-2
  [width height antennas]
  (let [length (count antennas)
        antenna-pairs (apply concat
                        (for [i (range 0 (dec length))]
                          (->> (range (inc i) length)
                               (mapv #(vector (nth antennas i) (nth antennas %))))))]
    (mapcat #(antinodes-on-line width height %) antenna-pairs)))


(defn part-2
  [input]
  (solve input find-antinodes-part-2))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 14
  (part-1 task-input)                                       ; => 220
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 34
  (part-2 task-input)                                       ; => 813
  (crit/quick-bench (part-2 task-input))

  )
