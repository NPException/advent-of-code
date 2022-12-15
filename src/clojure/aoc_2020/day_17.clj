(ns aoc-2020.day-17
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 17: Conway Cubes --- https://adventofcode.com/2020/day/17

(def task-input (u/slurp-resource "inputs/aoc_2020/day-17.txt"))

(def test-input ".#.\n..#\n###")


(defn parse-grid-line
  [additional-coordinates y line]
  (keep-indexed
    (fn [x e]
      (when (= \# e)
        (apply vector x y additional-coordinates)))
    line))

(defn parse-grid
  "returns a set of coordinates of active cubes"
  [input offsets]
  (->> (string/split-lines input)
       (map-indexed (partial parse-grid-line (->> offsets first (drop 2))))
       (mapcat identity)
       (into #{})))


(defn safe-add
  [a b]
  (+ (or a 0) b))


(defn process-active
  [offsets grid [new-grid seen-inactive-neighbours] cube-coordinate]
  (let [inactive-neighbours (->> offsets
                                (map #(mapv + % cube-coordinate)) ;; get absolute coordinates of neigbours
                                (filter (complement grid)))
        active-neighbour-count (- (count offsets) (count inactive-neighbours))]
    [(if (<= 2 active-neighbour-count 3)
       new-grid
       (disj new-grid cube-coordinate))
     (reduce
       #(update %1 %2 safe-add 1)
       seen-inactive-neighbours
       inactive-neighbours)]))


(defn process-inactive
  [new-grid [inactive-coordinate neighbours]]
  (if (= neighbours 3)
    (conj new-grid inactive-coordinate)
    new-grid))


(defn cycle-grid
  [offsets grid]
  (->> grid
       (reduce
         (partial process-active offsets grid)
         [grid {}])
       (apply reduce process-inactive)))


(defn find-solution
  [input offsets]
  (->> (parse-grid input offsets)
       (iterate (partial cycle-grid offsets))
       (take 7)
       last
       count))


;; part 1

(def offsets-3d
  (vec (for [x (range -1 2)
             y (range -1 2)
             z (range -1 2)
             :when (not= 0 x y z)]
         [x y z])))


(defn part-1
  [input]
  (find-solution input offsets-3d))


;; part 2

(def offsets-4d
  (vec (for [x (range -1 2)
             y (range -1 2)
             z (range -1 2)
             w (range -1 2)
             :when (not= 0 x y z w)]
         [x y z w])))


(defn part-2
  [input]
  (find-solution input offsets-4d))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 112
  (part-1 task-input)                                       ; => 237

  ;; Part 2
  (part-2 test-input)                                       ; => 848
  (part-2 task-input)                                       ; => 2448

  )
