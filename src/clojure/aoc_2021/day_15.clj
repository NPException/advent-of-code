(ns aoc-2021.day-15
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 15: Chiton --- https://adventofcode.com/2021/day/15

(def task-input (u/slurp-resource "inputs/aoc_2021/day-15.txt"))

(def test-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581\n")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv (fn [line] (mapv #(- (int %) 48) line)))))


(defn solve
  [input goal-fn risk-fn]
  (let [grid  (parse-input input)
        width (count grid)
        [max :as goal] (goal-fn width)
        path  (u/A*-search
                [0 0] #(= % goal)
                (fn [[x y]]
                  (cond-> '()
                    (> x 0) (conj [(dec x) y])
                    (< x max) (conj [(inc x) y])
                    (> y 0) (conj [x (dec y)])
                    (< y max) (conj [x (inc y)])))
                (fn [_] 0)
                ;; cost: the risk at the neighbouring position
                #(risk-fn grid width %2))]
    (->> (map #(risk-fn grid width %) path)
         (drop 1)
         (apply +))))


(defn part-1
  [input]
  (solve input
         (juxt dec dec)
         (fn [grid _width [x y]]
           (-> (nth grid y) (nth x)))))

(defn part-2
  [input]
  (solve input
         #(let [max (dec (* 5 %))]
            [max max])
         (fn [grid width [x y]]
           (let [risk (-> (nth grid (rem y width)) (nth (rem x width))
                          (+ (quot x width)
                             (quot y width)))]
             (if (> risk 9) (- risk 9) risk)))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 40
  (part-1 task-input)                                       ; => 673
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 315
  (part-2 task-input)                                       ; => 2893
  (quick-bench (part-2 task-input))

  )
