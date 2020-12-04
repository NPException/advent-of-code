(ns aoc-2020.day-3
  (:require [aoc-utils :as u]
            [clojure.string :as string]))

;; --- Day 3: Toboggan Trajectory --- https://adventofcode.com/2020/day/3

(def task-input
  (u/slurp-resource "inputs/aoc-2020/day-3.txt"))


(defn count-trees
  [input slope-right slope-down]
  (let [lines (string/split-lines input)
        w (count (first lines))]
    (->> lines
         ;; check down
         (map-indexed vector)
         (filter #(= 0 (mod (first %) slope-down)))
         ;; check right
         (map-indexed (fn [i [_ line]]
                        [(* slope-right i) line]))
         (filter (fn [[pos line]]
                   (= \# (nth line (mod pos w)))))
         count)))


;; alternative approach
(defn count-trees-2
  [input right down]
  (let [lines (string/split-lines input)
        width (count (first lines))]
    (->> (range (count lines))
         (filter #(= 0 (mod % down)))
         (map #(get-in lines [% (-> % (/ down) (* right) (mod width))]))
         (filter #(= \# %))
         count)))


(comment
  ;; Part 1
  (count-trees task-input 3 1)

  ;; Part 2
  (* (count-trees task-input 1 1)
     (count-trees task-input 3 1)
     (count-trees task-input 5 1)
     (count-trees task-input 7 1)
     (count-trees task-input 1 2))
  )
