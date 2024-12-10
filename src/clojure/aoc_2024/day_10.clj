(ns aoc-2024.day-10
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 10: Hoof It ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-10.txt"))

(def test-input "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732")

(defn parse-grid
  [text]
  (str/split-lines text))


(defn neighbours
  [grid [^long x ^long y]]
  [(u/grid-element grid (dec x) y nil)
   (u/grid-element grid (inc x) y nil)
   (u/grid-element grid x (dec y) nil)
   (u/grid-element grid x (inc y) nil)])

(defn next-trail-steps
  [grid from-coords next-height]
  (->> from-coords
       (mapcat #(neighbours grid %))
       (filterv (fn [[_ _ e]]
                  (= e next-height)))
       (distinct)))

(defn find-trail-heads
  [grid]
  (u/find-grid-elements grid (fn [[_ _ e]]
                               (= e \0))))


;; PART 1 ;;

(defn trail-head-score
  [grid head]
  (count
    (reduce
      (fn [coordinates next-height]
        (next-trail-steps grid coordinates next-height))
      [head]
      [\1 \2 \3 \4 \5 \6 \7 \8 \9])))

(defn part-1
  [input]
  (let [grid (parse-grid input)]
    (->> (find-trail-heads grid)
         (mapv #(trail-head-score grid %))
         (apply +))))


;; PART 2 ;;

(defn trail-rating
  [grid step [next-height & more-heights]]
  ; If it's the last step, return 1. else sum up sub paths
  (if (nil? next-height)
    1
    (->> (neighbours grid step)
         (filterv (fn [[_ _ e]]
                    (= e next-height)))
         (mapv #(trail-rating grid % more-heights))
         (apply +))))

(defn part-2
  [input]
  (let [grid (parse-grid input)]
    (->> (find-trail-heads grid)
         (mapv #(trail-rating grid % [\1 \2 \3 \4 \5 \6 \7 \8 \9]))
         (apply +))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 36
  (part-1 task-input)                                       ; => 472
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 81
  (part-2 task-input)                                       ; => 969
  (crit/quick-bench (part-2 task-input))

  )
