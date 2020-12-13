(ns aoc-2020.day-13
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 13: Shuttle Search --- https://adventofcode.com/2020/day/13

(def task-input (u/slurp-resource "inputs/aoc_2020/day-13.txt"))

(def test-input "939\n7,13,x,x,59,x,31,19")


(defn parse-input
  [^String input]
  (let [[time & bus-ids] (string/split input #"[\n,]")]
    (->> (remove #(= % "x") bus-ids)
         (mapv (juxt (constantly (u/parse-long time))
                     u/parse-long)))))


(defn minutes-to-wait
  [[time bus-id]]
  (- bus-id (mod time bus-id)))


(defn part-1
  [input]
  (->> (parse-input input)
       (sort-by minutes-to-wait)
       first
       ((juxt second minutes-to-wait))
       (apply *)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 295
  (part-1 task-input)                                       ; => 2092

  ;; Part 2
  (part-2 test-input)
  (part-2 task-input)

  )
