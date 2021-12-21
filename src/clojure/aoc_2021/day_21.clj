(ns aoc-2021.day-21
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 21: Dirac Dice --- https://adventofcode.com/2021/day/21

(def task-input (u/slurp-resource "inputs/aoc_2021/day-21.txt"))
(def test-input "Player 1 starting position: 4\nPlayer 2 starting position: 8")

(defn parse-input
  [^String input]
  (let [^long i1 (str/index-of input \:)
        ^long i2 (str/index-of input \: (inc i1))]
    [(- (int (.charAt input (+ i1 2))) 49)                  ;; sub by 49 to get 0-based position
     (- (int (.charAt input (+ i2 2))) 49)]))

(defn part-1
  [input]
  (let [[^long p1 ^long p2] (parse-input input)]
    (loop [pa p1, pb p2, sa 0, sb 0, rolls 0
           [^long d1 ^long d2 ^long d3 & more-dice] (cycle (range 1 101))]
      (let [pa' (rem (+ pa d1 d2 d3) 10)
            sa' (+ sa (inc pa'))]
        (if (>= sa' 1000)
          (* (+ rolls 3) sb)
          (recur pb pa' sb sa' (+ rolls 3) more-dice))))))


(def dirac-rolls [[3 1] [4 3] [5 6] [6 7] [7 6] [8 3] [9 1]]) ;; die value, and number of combinations that result in that value

(def play
  (memoize
    (fn [^long pos1 ^long pos2 ^long score1 ^long score2]
      (if (>= score2 21)
        [0 1]
        (loop [wins1 0, wins2 0, roll-index 0]
          (if (>= roll-index 7)
            [wins1 wins2]
            (let [[^long move ^long n] (dirac-rolls roll-index)
                  pos1' (rem (+ pos1 move) 10)
                  [^long w2 ^long w1] (play pos2 pos1' score2 (+ score1 (inc pos1')))]
              (recur (+ wins1 (* n w1))
                     (+ wins2 (* n w2))
                     (inc roll-index)))))))))

(defn part-2
  [input]
  (let [[p1 p2] (parse-input input)]
    (apply max (play p1 p2 0 0))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 739785
  (part-1 task-input)                                       ; => 504972
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 444356092776315
  (part-2 task-input)                                       ; => 446968027750017
  (quick-bench (part-2 task-input))

  )
