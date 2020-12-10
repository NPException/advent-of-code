(ns aoc-2020.day-10
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 10: Adapter Array --- https://adventofcode.com/2020/day/10

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-10.txt"))

(def numbers (->> task-input
                  string/split-lines
                  (mapv #(Long/parseLong %))))


(def test-numbers-1 (->> "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
                         string/split-lines
                         (mapv u/parse-long)))

(def test-numbers-2 (->> "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
                         string/split-lines
                         (mapv u/parse-long)))


(defn calc-differences
  [nums]
  (->> (conj nums 0 (+ 3 (apply max nums)))
       (sort >)
       (partition 2 1)
       (map #(apply - %))))


(defn part-1
  [nums]
  (->> (calc-differences nums)
       frequencies
       (#(select-keys % [1 3]))
       vals
       (apply *)))


(defn part-2
  [nums]
  (->> (calc-differences nums)
       ;; TODO
       ))


(comment
  ;; Part 1 => 2516
  (part-1 numbers)
  ;; => 35
  (part-1 test-numbers-1)
  ;; => 220
  (part-1 test-numbers-2)

  ;; Part 2 =>
  (part-2 numbers)
  ;; => 8
  (part-2 test-numbers-1)
  ;; => 19208
  (part-2 test-numbers-2)
  )
