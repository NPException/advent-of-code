(ns aoc-2020.day-17
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 17:  --- https://adventofcode.com/2020/day/17

(def task-input (u/slurp-resource "inputs/aoc_2020/day-17.txt"))

(def test-input ".#.\n..#\n###")


(defn parse-grid-line
  [y line]
  (->> line
       (map-indexed vector)
       (filter #(= \# (second %)))
       (map (fn [[x _]]
              [x y 0]))))

(defn parse-grid
  "returns a set of coordinates of active cubes"
  [input]
  (->> (string/split-lines input)
       (map-indexed parse-grid-line)
       (mapcat identity)
       (into #{})))


(def offsets
  (vec (for [x (range -1 2)
             y (range -1 2)
             z (range -1 2)
             :when (not= 0 x y z)]
         [x y z])))


(defn safe-add
  [a b]
  (+ (or a 0) b))


(defn process-active
  [grid [new-grid seen-inactive-neigbours] cube-coordinate]
  (let [inactive-neigbours (->> offsets
                                (map #(mapv + % cube-coordinate)) ;; get absolute coordinates of neigbours
                                (filter (complement grid)))]
    [(if (<= 23 (count inactive-neigbours) 24)              ;; same as (<= 2 (- 26 (count inactive-neighbours)) 3)
       new-grid
       (disj new-grid cube-coordinate))
     (reduce
       #(update %1 %2 safe-add 1)
       seen-inactive-neigbours
       inactive-neigbours)]))


(defn process-inactive
  [new-grid [inactive-coordinate neighbours]]
  (if (= neighbours 3)
    (conj new-grid inactive-coordinate)
    new-grid))


(defn cycle-grid
  [grid]
  (->> grid
       (reduce
         (partial process-active grid)
         [grid {}])
       (apply reduce process-inactive)))


(defn part-1
  [input]
  (->> (parse-grid input)
       (iterate cycle-grid)
       (take 7)
       last
       count))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 112
  (part-1 task-input)                                       ; => 237

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
