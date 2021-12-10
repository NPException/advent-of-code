(ns aoc-2021.day-9
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 9: Smoke Basin --- https://adventofcode.com/2021/day/9

(def task-input (u/slurp-resource "inputs/aoc_2021/day-9.txt"))

(def test-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")


(defn point-at
  ([grid [x y]] (point-at grid x y))
  ([grid x y]
   (-> grid (nth y "") (nth x \9))))

(defn neighbours
  ([[x y]] (neighbours x y))
  ([x y]
   [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

(defn low-point?
  [grid x y]
  (let [value (int (point-at grid x y))]
    (->> (neighbours x y)
         (mapv #(point-at grid %))
         (every? #(< value (int %))))))

(defn find-low-points
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (low-point? grid x y)]
    [x y]))


(defn part-1
  [input]
  (let [grid (str/split-lines input)
        lowpoints (find-low-points grid)]
    (apply + (map #(- (int (point-at grid %)) 47) lowpoints))))


(defn calc-basin-size
  [grid lowpoint]
  (loop [[point & remaining] [lowpoint]
         seen #{point}]
    (if (nil? point)
      (count seen)
      (let [basin-neighbours (->> (neighbours point)
                                  (remove #(or (seen %) (= \9 (point-at grid %)))))]
        (recur (into remaining basin-neighbours)
               (into seen basin-neighbours))))))

(defn part-2
  [input]
  (let [grid (str/split-lines input)
        lowpoints (find-low-points grid)]
    (->> (mapv #(calc-basin-size grid %) lowpoints)
         (sort >)
         (take 3)
         (apply *))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 15
  (part-1 task-input)                                       ; => 417
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 1134
  (part-2 task-input)                                       ; => 1148965
  (quick-bench (part-2 task-input))

  )
