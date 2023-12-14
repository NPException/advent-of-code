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

(defn vertical-mirror-at?
  [row i]
  (let [left  (subs row 0 i)
        right (subs row i)]
    (if (> (count left) (count right))
      (str/ends-with? left (str/reverse right))
      (str/starts-with? right (str/reverse left)))))

(defn vertical-mirror-index
  [rows illegal-index]
  (->> (range 1 (count (first rows)))
       (u/first-match (fn [i]
                        (and (not= i illegal-index)
                             (every? #(vertical-mirror-at? % i) rows))))))


(defn horizontal-mirror-at?
  [rows i]
  (let [top    (vec (take i rows))
        bottom (vec (drop i rows))]
    (if (> (count top) (count bottom))
      (u/ends-with? top (vec (rseq bottom)))
      (u/starts-with? bottom (vec (rseq top))))))

(defn horizontal-mirror-index
  [rows illegal-index]
  (->> (range 1 (count rows))
       (u/first-match #(and (not= % illegal-index)
                            (horizontal-mirror-at? rows %)))))


(defn find-mirror
  "Finds the mirror axis, and returns a vector of [rows-above-mirror columns-left-of-mirror].
  If the mirror is vertical, rows will be 0. If the mirror is horizontal, columns will be 0."
  ([rows]
   (find-mirror rows nil))
  ([rows [r c :as _forbidden]]
   (if-let [mi (vertical-mirror-index rows r)]
     [mi 0]
     (when-let [mi (horizontal-mirror-index rows c)]
       [0 mi]))))


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
       (mapv find-smudged-mirror)
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
  (part-2 task-input)                                       ; => 37478
  (crit/quick-bench (part-2 task-input))

  )
