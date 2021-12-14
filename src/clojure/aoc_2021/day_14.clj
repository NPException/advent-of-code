(ns aoc-2021.day-14
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 14: Extended Polymerization --- https://adventofcode.com/2021/day/14

(def task-input (u/slurp-resource "inputs/aoc_2021/day-14.txt"))

(def test-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")

(defn parse-polymer
  [s]
  [(frequencies s)
   (->> (partition 2 1 s)
        (map str/join)
        frequencies)])

(defn parse-insertions
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #" -> "))
       (mapv (fn [[pair c]]
               (let [c (first c)
                     p1 (str (first pair) c)
                     p2 (str c (second pair))
                     safe-add (fnil + 0)]
                 (fn [[counts new-polymer] polymer]
                   (let [n (or (polymer pair) 0)]
                     [(update counts c safe-add n)
                      (-> (update new-polymer p1 safe-add n)
                          (update p2 safe-add n))])))))))

(defn parse-input
  [input]
  (let [[polymer insertions] (str/split input #"\n\n")]
    [(parse-polymer polymer)
     (parse-insertions insertions)]))

(defn step
  [inserts [counts polymer]]
  (reduce
    (fn [new f]
      (f new polymer))
    [counts {}]
    inserts))

(defn solve
  [input steps]
  (let [[polymer inserts] (parse-input input)
        counts (-> (iterate #(step inserts %) polymer)
                   (nth steps)
                   first vals sort)]
    (- (last counts) (first counts))))


(defn part-1
  [input]
  (solve input 10))

(defn part-2
  [input]
  (solve input 40))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1588
  (part-1 task-input)                                       ; => 3230
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 2188189693529
  (part-2 task-input)                                       ; => 3542388214529
  (quick-bench (part-2 task-input))

  )
