(ns aoc-2021.day-4
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]
            [clojure.edn :as edn]))

;; --- Day 4: Giant Squid --- https://adventofcode.com/2021/day/4


(def task-input (u/slurp-resource "inputs/aoc_2021/day-4.txt"))

(def test-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")


(defn parse-board
  [lines]
  (let [rows (mapv #(edn/read-string (str "[" % "]")) lines)
        goals (concat
                (mapv set rows)                             ;; rows
                (for [x (range 5)]                          ;; columns
                  (set (mapv #(nth % x) rows))))]
    {:rows rows
     :goals (vec goals)}))

(defn parse-input
  [input]
  (let [[numbers & lines] (str/split-lines input)]
    {:numbers (edn/read-string (str "[" numbers "]"))
     :boards (->> (partition-by empty? lines)
                  (remove #(empty? (first %)))
                  (mapv parse-board))}))


(defn update-board
  [board num]
  (let [goals (mapv #(disj % num) (:goals board))
        winner? (some empty? goals)]
    (cond-> (assoc board :goals goals)
      winner? (assoc :winning-num num))))


(defn find-winner
  [input advance-rf]
  (let [{:keys [numbers boards]} (parse-input input)
        winner (reduce advance-rf boards numbers)]
    (->> (:goals winner)
         (apply concat)
         (set)
         (apply +)
         (* (:winning-num winner)))))


(defn part-1
  [input]
  (find-winner input
               (fn [boards num]
                 (let [new-boards (mapv #(update-board % num) boards)]
                   (if-let [winner (first (filterv :winning-num new-boards))]
                     (reduced winner)
                     new-boards)))))


(defn part-2
  [input]
  (find-winner input
               (fn [boards num]
                 (let [new-boards (mapv #(update-board % num) boards)
                       non-winners (filterv (complement :winning-num) new-boards)]
                   (if (empty? non-winners)
                     (reduced (first new-boards))
                     non-winners)))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 4512
  (part-1 task-input)                                       ; => 21607

  ;; Part 2
  (part-2 test-input)                                       ; => 1924
  (part-2 task-input)                                       ; => 19012

  )
