(ns aoc-2020.day-11
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 11: Seating System --- https://adventofcode.com/2020/day/11

(def task-input (u/slurp-resource "inputs/aoc_2020/day-11.txt"))

(def test-input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")


;; for debugging
(defn print-seats
  [seat-map]
  (let [seat-map (into {} (map #(vector (key %) (or (and (val %) \#) \L))) seat-map)
        w (->> seat-map keys (map first) (apply max) inc)
        h (->> seat-map keys (map second) (apply max) inc)]
    (dotimes [y h]
      (dotimes [x w]
        (print (seat-map [x y] \.)))
      (println))))


(defn parse-seat-line
  [y line]
  (->> line
       (map-indexed #(vector [%1 y] (= \. %2)))              ;; input contains no #, so we only need to distinguish between L and .
       (filter (comp not second))))

(defn parse-seats
  ;; returns a map of seat coordinates to occupied state (initially false)
  [input]
  (->> input
       string/split-lines
       (map-indexed parse-seat-line)
       (mapcat identity)
       (into {})))


(def neighbour-offsets
  (vec
    (for [y (range -1 2)
          x (range -1 2)
          :when (or (not= y 0)
                    (not= x 0))]
      [x y])))


(defn count-neighbours
  "Counts occupied seats adjacent to the given coordinates"
  [seats [x y]]
  (reduce
    (fn [acc [ox oy]]
      (if (seats [(+ x ox) (+ y oy)])
        (inc acc)
        acc))
    0
    neighbour-offsets))


(defn swap-seat-state?
  [occupied? neighbours]
  (or (and (not occupied?)
           (= neighbours 0))
      (and occupied?
           (>= neighbours 4))))


(defn process-seat
  [seats [coords occupied? :as seat]]
  (if (swap-seat-state? occupied? (count-neighbours seats coords))
    [coords (not occupied?)]
    seat))


(defn process-seats
  [seats]
  (into {} (map #(process-seat seats %))
        seats))


(defn part-1
  [input]
  (->> (parse-seats input)
       (iterate process-seats)
       (partition 2 1)
       (take-while #(apply not= %))
       last
       second
       vals
       (filter true?)
       count))


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
