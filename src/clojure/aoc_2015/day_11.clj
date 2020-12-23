(ns aoc-2015.day-11
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 11: Corporate Policy --- https://adventofcode.com/2015/day/11

(def task-input (u/slurp-resource "inputs/aoc_2015/day-11.txt"))


(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def char-values
  (->> alphabet
       (map-indexed #(vector %2 %1))
       (into {})))


(defn inc-numeric-reversed-pw
  [numbers]
  (loop [[num & remain] numbers
         done (list)]
    (if (nil? num)
      done
      (let [new-num (inc num)]
        (cond
          (<= new-num 25) (into (conj done new-num) remain)
          (empty? remain) (into done [0 0])
          :else (recur remain (conj done 0)))))))

(defn inc-pw
  [pw]
  (->> (reverse pw)
       (map char-values)
       inc-numeric-reversed-pw
       (map #(nth alphabet %))
       string/join))


(defn rule-1
  "Passwords must include one increasing straight of at least three letters"
  [pw]
  (->> (map char-values pw)
       (partition 3 1)
       (filter (fn [[a b c]]
                 (= a (dec b) (- c 2))))
       seq))

(defn rule-2
  "Passwords may not contain the letters i, o, or l"
  [pw]
  (not-any? #{\i \o \l} pw))

(defn rule-3
  "Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz."
  [pw]
  (->> (partition 2 1 pw)
       (filter #(apply = %))
       (partition-by identity)
       (mapcat #(take-nth 2 %))
       count
       (< 1)))


(defn next-pw
  [input]
  (->> input
       (iterate inc-pw)
       (drop 1)
       (filter (u/and-fn rule-1 rule-2 rule-3))
       first))


(comment
  ;; Part 1
  (next-pw task-input)                                       ; => cqjxxyzz

  ;; Part 2
  (next-pw "cqjxxyzz")                                       ; => cqkaabcc

  )
