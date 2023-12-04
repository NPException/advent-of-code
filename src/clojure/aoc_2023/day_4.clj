(ns aoc-2023.day-4
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 4: Scratchcards ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-4.txt"))

(def test-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv (fn [line]
               (let [[_ winning own] (re-find #":([^|]+)\|(.*)" line)]
                 [(read-string (str "#{" winning "}"))
                  (read-string (str "#{" own "}"))])))))

(defn num-matches
  [[winning owned]]
  (count (set/intersection winning owned)))

(defn part-1
  [input]
  (->> (parse-input input)
       (map num-matches)
       (remove zero?)
       (map #(long (math/pow 2 (dec %))))
       (apply +)))


(defn add-cards
  [counts card-index matches]
  (if (zero? matches)
    counts
    (let [factor (nth counts card-index)
          next-index (inc card-index)]
      (reduce
        (fn [acc index]
          (if-let [n (get acc index)]
            (assoc acc index (+ n factor))
            acc))
        counts
        (range next-index (+ next-index matches))))))

(defn part-2
  [input]
  (let [cards (parse-input input)]
    (loop [counts (vec (repeat (count cards) 1))
           i 0]
      (if (>= i (count cards))
        (apply + counts)
        (let [matches (num-matches (nth cards i))]
          (recur (add-cards counts i matches)
            (inc i)))))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 13
  (part-1 task-input)                                       ; => 20829
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 30
  (part-2 task-input)                                       ; => 12648035
  (crit/quick-bench (part-2 task-input))

  )
