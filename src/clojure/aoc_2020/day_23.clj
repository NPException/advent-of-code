(ns aoc-2020.day-23
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 23: Crab Cups --- https://adventofcode.com/2020/day/23

(def task-input (u/slurp-resource "inputs/aoc_2020/day-23.txt"))

(def test-input "389125467")


(defn parse-input
  [input]
  (->> input
       (map str)
       (map parse-long)))


(defn create-ring
  ^ints [nums]
  (->> (concat nums [(first nums)])
       (partition 2 1)
       (reduce
         (fn [ring [cup next]]
           (aset-int ring cup next)
           ring)
         (int-array (inc (count nums))))))


(defn cup-dec
  [max-num current a b c]
  (loop [dest (if (= current 1) max-num (dec current))]
    (if (or (= dest a)
            (= dest b)
            (= dest c))
      (recur (if (= dest 1) max-num (dec dest)))
      dest)))


(defn play
  [max-num times ^ints ring current]
  (loop [current current
         n times]
    (when (not= n 0)
      (let [a (aget ring current)
            b (aget ring a)
            c (aget ring b)
            destination (cup-dec max-num current a b c)
            ;; point current at cup after c
            next-cup (aset-int ring current (aget ring c))]
        ;; point c at where destination points to
        (aset-int ring c (aget ring destination))
        ;; point destination at beginning of our 3 numbers
        (aset-int ring destination a)
        ;; next turn
        (recur next-cup (dec n))))))


(defn part-1
  [input]
  (let [numbers (parse-input input)
        ring (create-ring numbers)]
    (play 9 100 ring (first numbers))
    (->> 1
         (iterate #(aget ring %))
         (drop 1)
         (take 8)
         string/join)))


(defn part-2
  [input]
  (let [numbers (parse-input input)
        ring (create-ring (concat numbers (range 10 1000001)))]
    (play 1000000
          10000000
          ring
          (first numbers))
    (->> 1
         (iterate #(aget ring %))
         (drop 1)
         (take 2)
         (apply *))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 67384529
  (part-1 task-input)                                       ; => 45798623

  ;; Part 2
  (part-2 test-input)                                       ; => 149245887792
  (part-2 task-input)                                       ; => 235551949822

  )
