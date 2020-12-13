(ns aoc-2020.day-11
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 11: Seating System --- https://adventofcode.com/2020/day/11

(def task-input (u/slurp-resource "inputs/aoc_2020/day-11.txt"))

(def test-input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")



(defn process-seat
  [tracers leave-threshold seats y x state]
  (if (= state \.)
    state
    (let [neighbours (transduce (map #(% seats x y)) + tracers)]
      (or (and (= state \L)
               (= neighbours 0)
               \#)
          (and (= state \#)
               (>= neighbours leave-threshold)
               \L)
          state))))


(defn process-row
  [tracers leave-threshold seats y row]
  (into []
        (map-indexed (partial process-seat tracers leave-threshold seats y))
        row))


(defn process-seats
  [tracers leave-threshold seats]
  (into []
        (map-indexed (partial process-row tracers leave-threshold seats))
        seats))


(defn find-equilibrium-seats
  [seats build-tracer leave-threshold]
  (let [tracers (vec (for [ox (range -1 2)
                           oy (range -1 2)
                           :when (not= ox oy 0)]
                       (build-tracer ox oy)))]
    (->> seats
         (iterate (partial process-seats tracers leave-threshold))
         (partition 2 1)
         (take-while #(apply not= %))
         last
         second
         (apply concat)
         (filter #(= % \#))
         count)))



;; part 1

(defn build-tracer-p1
  "Returns a tracer function, which takes the seats and a coordinate,
  then returns 1 if there is an occupied seat in the offset direction, else 0."
  [ox oy]
  (fn [seats x y]
    (if (= \# (get-in seats [(+ y oy) (+ x ox)]))
      1
      0)))

(defn part-1
  [input]
  (find-equilibrium-seats (string/split-lines input) build-tracer-p1 4))



;; part 2

(defn build-tracer-p2
  "Returns a tracer function, which takes the seats and a coordinate,
  then returns 1 if there is an occupied seat in the offset direction, else 0."
  [ox oy]
  (fn [seats x y]
    (loop [x (+ x ox)
           y (+ y oy)]
      (let [state (get-in seats [y x])]
        (or (and (or (= state \L) (nil? state)) 0)
            (and (= state \#) 1)
            (recur (+ x ox) (+ y oy)))))))


(defn part-2
  [input]
  (find-equilibrium-seats (string/split-lines input) build-tracer-p2 5))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 37
  (part-1 task-input)                                       ; => 2275

  ;; Part 2
  (part-2 test-input)                                       ; => 26
  (part-2 task-input)                                       ; => 2121

  )
