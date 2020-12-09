(ns aoc-2020.day-9
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 9: Encoding Error --- https://adventofcode.com/2020/day/9

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-9.txt"))

(def input-numbers (->> task-input
                        string/split-lines
                        (mapv u/parse-long)))


;; part 1 functions

(defn invalid?
  [[n prev-nums]]
  (empty?
    (for [x prev-nums
          y prev-nums
          :when (and (not= x y)
                     (= n (+ x y)))]
      :dummy)))


(defn validation-groups
  [numbers preamble]
  (->> (partition (inc preamble) 1 numbers)
       (map (juxt last butlast))))


;; part 2 functions

(defn find-consecutive-nums-adding-up-to
  [numbers n]
  (->> (range 2 (count numbers))
       (map #(take % numbers))
       (map #(vector % (apply + %)))
       (drop-while #(< (second %) n))
       (take 1)
       (filter #(= (second %) n))
       ffirst))



(comment
  ;; Part 1 => 32321523
  (->> (validation-groups input-numbers 25)
       (filter invalid?)
       ffirst)
  ;; Part 2 => 4794981
  (->> (loop [remaining-nums input-numbers]
         (or (find-consecutive-nums-adding-up-to remaining-nums 32321523)
             (recur (rest remaining-nums))))
       (apply (juxt min max))
       (apply +))
  )
