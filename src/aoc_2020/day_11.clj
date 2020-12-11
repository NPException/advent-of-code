(ns aoc-2020.day-11
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 11: Seating System --- https://adventofcode.com/2020/day/11

(def task-input (u/slurp-resource "inputs/aoc_2020/day-11.txt"))

(def test-input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")


;; for debugging
(defn print-seats
  [seats]
  (doseq [row seats]
    (println (apply str row))))


(defn parse-seats
  [input]
  (->> input
       string/split-lines
       (mapv vec)))


(defn process-seat
  [neigbour-counter leave-threshold seats y x state]
  (if (= state \.)
    state
    (let [neighbours (neigbour-counter seats x y)]
      (or (and (= state \L)
               (= neighbours 0)
               \#)
          (and (= state \#)
               (>= neighbours leave-threshold)
               \L)
          state))))


(defn process-row
  [neigbour-counter leave-threshold seats y row]
  (into []
        (map-indexed (partial process-seat neigbour-counter leave-threshold seats y))
        row))


(defn process-seats
  [neigbour-counter leave-threshold seats]
  (into []
        (map-indexed (partial process-row neigbour-counter leave-threshold seats))
        seats))


(defn find-equilibrium-seats
  [seats neighbour-counter leave-threshold]
  (->> seats
       (iterate (partial process-seats neighbour-counter leave-threshold))
       (partition 2 1)
       (take-while #(apply not= %))
       last
       second
       (apply concat)
       (filter #(= % \#))
       count))



;; part 1

(def neighbour-offsets-p1
  (vec
    (for [y (range -1 2)
          x (range -1 2)
          :when (not= y x 0)]
      [x y])))

(defn count-neighbours-p1
  "Counts occupied seats adjacent to the given coordinates"
  [seats x y]
  (transduce
    (map (fn [[ox oy]]
           (if (= \# (get-in seats [(+ y oy) (+ x ox)]))
             1
             0)))
    +
    neighbour-offsets-p1))

(defn part-1
  [input]
  (find-equilibrium-seats (parse-seats input) count-neighbours-p1 4))



;; part 2

(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 37
  (part-1 task-input)                                       ; => 2275

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
