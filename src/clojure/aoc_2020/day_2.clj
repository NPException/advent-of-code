(ns aoc-2020.day-2
  (:require [aoc-utils :as u]))

;; --- Day 2: Password Philosophy --- https://adventofcode.com/2020/day/2

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-2.txt"))


(defn line->rule-set
  [[_ n1 n2 c password]]
  [(Integer/parseInt n1)
   (Integer/parseInt n2)
   (first c)
   password])


(defn count-valid-passwords
  [input valid-password?]
  (->> input
       (re-seq #"(\d+)-(\d+) (.): (.+)")
       (map line->rule-set)
       (filter #(apply valid-password? %))
       count))


;; validator for part 1

(defn matching-char-count?
  [n1 n2 c password]
  (<= n1
      (get (frequencies password) c 0)
      n2))


;; validator for part 2

(defmacro xor
  [a b]
  `(or (and ~a (not ~b))
       (and ~b (not ~a))))

(defn matching-xor-char-position?
  [n1 n2 c password]
  (let [c1 (nth password (dec n1))
        c2 (nth password (dec n2))]
    (xor (= c1 c)
         (= c2 c))))


(comment
  ;; Part 1
  (count-valid-passwords task-input matching-char-count?)
  ;; Part 2
  (count-valid-passwords task-input matching-xor-char-position?)
  )
