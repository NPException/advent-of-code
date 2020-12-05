(ns aoc-2020.day-5
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 5: Binary Boarding --- https://adventofcode.com/2020/day/5

(defn parse-seat-id
  [seat]
  (-> (string/replace seat #"." {"F" "0", "B" "1", "L" "0", "R" "1"})
      (Integer/parseInt 2)))


(def all-ids
  (->> (u/slurp-resource "inputs/aoc_2020/day-5.txt")
       string/split-lines
       (mapv parse-seat-id)))


(defn find-my-seat
  [ids]
  (->> (sort ids)
       (#(interleave % (rest %)))
       (partition 2)
       (filter (fn [[a b]] (not= (inc a) b)))
       ffirst
       inc))


;; more efficient approach (credits to Jezza)
(defn find-my-seat-2
  [ids]
  (->> (range (apply min ids) (apply max ids))
       (filter (complement (set ids)))
       first))



(comment
  ;; Part 1
  (apply max all-ids)
  ;; Part 2
  (find-my-seat all-ids)
  )
