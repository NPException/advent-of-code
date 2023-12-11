(ns aoc-2023.day-11
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 11: Cosmic Expansion ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-11.txt"))

(def test-input "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....")

(defn parse-input
  [input]
  (str/split-lines input))

(defn non-galaxy-lines
  [grid]
  (->> grid
       (keep-indexed (fn [i line]
                       (when (not (some #(= % \#) line)) i)))
       (into #{})))

(defn distance-1d
  ^long
  [expand? ^long expansion ^long a ^long b]
  (let [step (long (math/signum (- b a)))]
    (loop [d   0
           pos a]
      (if (= pos b)
        d
        (let [pos' (+ pos step)]
          (recur (if (expand? pos')
                   (+ d expansion)
                   (inc d))
            pos'))))))

(defn distance-2d
  [expand-x? expand-y? expansion [x1 y1] [x2 y2]]
  (+ (distance-1d expand-x? expansion x1 x2)
     (distance-1d expand-y? expansion y1 y2)))


(defn solve
  [input expansion]
  (let [grid         (parse-input input)
        expand-y?    (non-galaxy-lines grid)
        expand-x?    (non-galaxy-lines (u/transpose grid))
        galaxies     (->> (u/grid-elements grid)
                          (filter #(= \# (nth % 2)))
                          (map (fn [[x y]] [x y])))
        galaxy-pairs (into #{}
                       (for [a galaxies
                             b galaxies
                             :when (not= a b)]
                         #{a b}))]
    (->> galaxy-pairs
         (map #(distance-2d expand-x? expand-y? expansion (first %) (second %)))
         (apply +))))



(comment
  ;; Part 1
  (solve test-input 2)                                      ; => 374
  (solve task-input 2)                                      ; => 9563821
  (crit/quick-bench (solve task-input 2))

  ;; Part 2
  (solve test-input 100)                                    ; => 8410
  (solve task-input 1000000)                                ; => 1000000
  (crit/quick-bench (solve task-input 1000000))

  )
