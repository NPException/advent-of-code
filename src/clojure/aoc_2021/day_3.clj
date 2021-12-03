(ns aoc-2021.day-3
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 3: Binary Diagnostic --- https://adventofcode.com/2021/day/3

(def task-input (str/split-lines (u/slurp-resource "inputs/aoc_2021/day-3.txt")))

(def test-input (str/split-lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"))


;; part 1 functions

(defn most-common-bit
  [& bits]
  (let [{ones \1 zeros \0} (u/frequencies+ bits)]           ;; using a custom 'frequencies' impl for a bit more speed
    (if (>= (or ones 0) (or zeros 0)) \1 \0)))

(defn part-1
  [codes]
  (let [gamma-bits (apply mapv most-common-bit codes)
        gamma-rate (u/parse-binary (str/join gamma-bits))
        epsilon-rate (->> (mapv #(if (= % \0) \1 \0) gamma-bits)
                          (str/join)
                          (u/parse-binary))]
    (* gamma-rate epsilon-rate)))


;; part 2 functions

(defn find-rating
  [codes select-fn]
  (loop [[code & rem :as codes] codes
         n 0]
    (if-not rem
      (u/parse-binary code)
      (let [most-common (apply most-common-bit (mapv #(nth % n) codes))]
        (recur (select-fn #(= most-common (nth % n)) codes)
               (inc n))))))

(defn part-2
  [codes]
  (* (find-rating codes filter)                             ;; oxygen generator rating
     (find-rating codes remove)))                           ;; CO2 scrubber rating


(comment
  (part-1 test-input)                                       ;; 198
  (part-1 task-input)                                       ;; 4191876

  (part-2 test-input)                                       ;; 230
  (part-2 task-input)                                       ;; 3414905
  ;
  )
