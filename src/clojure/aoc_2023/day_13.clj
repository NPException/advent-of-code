(ns aoc-2023.day-13
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 13: Point of Incidence ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-13.txt"))

(def test-input "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#")

(defn mirrored-at?
  [row i]
  (let [left  (subs row 0 i)
        right (subs row i)]
    (if (> (count left) (count right))
      (str/ends-with? left (str/reverse right))
      (str/starts-with? right (str/reverse left)))))

(defn mirror-axis?
  [rows i]
  (every? #(mirrored-at? % i) rows))

(defn mirror-index
  [rows]
  (->> (range 1 (count (first rows)))
       (u/first-match #(mirror-axis? rows %))))

(defn find-mirror
  "Finds the mirror axis, and returns a vector of [rows-above-mirror columns-left-of-mirror].
  If the mirror is vertical, rows will be 0. If the mirror is horizontal, columns will be 0."
  ([rows]
   (find-mirror rows [0 0]))
  ([rows [r c :as _forbidden]]
   (let [mi (mirror-index rows)]
     (if (and mi (not= mi r))
       [mi 0]
       (let [mi (->> (mapv str/reverse rows)
                     (u/transpose)
                     (mapv #(apply str %))
                     (mirror-index))]
         (when (and mi (not= mi c))
           [0 mi]))))))


(defn part-1
  [input]
  (->> (str/split input #"\n\n")
       (mapv str/split-lines)
       (mapv find-mirror)
       (mapv (fn [[columns rows]]
               (+ columns (* 100 rows))))
       (apply +)))


(defn find-smudged-mirror
  [rows]
  (let [smudged   (find-mirror rows)
        char-rows (mapv vec rows)]
    (->> (for [y (range (count char-rows))
               x (range (count (first char-rows)))]
           (->> (update-in char-rows [y x] {\. \#, \# \.})
                (mapv #(apply str %))))
         (keep #(find-mirror % smudged))
         (first))))


(defn part-2
  [input]
  (->> (str/split input #"\n\n")
       (mapv str/split-lines)
       (pmap find-smudged-mirror)
       (mapv (fn [[columns rows]]
               (+ columns (* 100 rows))))
       (apply +)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 405
  (part-1 task-input)                                       ; => 30575
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 400
  (part-2 task-input)                                       ; FIXME => crashes. if I filter out nil results, its 29059 and that's too low ...
  (crit/quick-bench (part-2 task-input))

  )
