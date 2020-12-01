(ns aoc-2020.day-1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; --- Day 1: Report Repair ---

(def task-input
  (-> (io/resource "inputs/aoc-2020/day-1.txt")
      slurp
      (#(str "[" % "]"))
      edn/read-string))


(defn calc-2 [input]
  (let [indexed-input (map-indexed vector input)]
    (first
      (for [[ai a] indexed-input
            [bi b] indexed-input
            :when (and (> bi ai)
                       (= 2020 (+ a b)))]
        (* a b)))))


;; --- Part Two ---

(defn calc-3 [input]
  (let [indexed-input (map-indexed vector input)]
    (first
      (for [[ai a] indexed-input
            [bi b] indexed-input
            [ci c] indexed-input
            :when (and (> ci bi ai)
                       (= 2020 (+ a b c)))]
        (* a b c)))))



(comment
  (calc-2 task-input)
  (calc-3 task-input)
  )
