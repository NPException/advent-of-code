(ns aoc-2021.day-21
  (:use [criterium.core])
  (:require [aoc-utils :as u])
  (:import (java.util HashMap Map)))

;; --- Day 21: Dirac Dice --- https://adventofcode.com/2021/day/21

(def task-input (u/slurp-resource "inputs/aoc_2021/day-21.txt"))
(def test-input "Player 1 starting position: 4\nPlayer 2 starting position: 8")

(defn parse-input
  [^String input]
  [(- (int (.charAt input 28)) 49)                          ;; sub by 49 to get 0-based position
   (- (int (.charAt input 58)) 49)])

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

(defn play
    ([[pos1 pos2]]
     (play [pos1 pos2 0 0] (HashMap.)))
    ([[^long pos1 ^long pos2 ^long score1 ^long score2] ^Map cache]
     (if (>= score2 21)
       [0 1]
       (loop [wins1 0, wins2 0, roll-index 0]
         (if (>= roll-index 7)
           [wins1 wins2]
           (let [[^long move ^long n] (dirac-rolls roll-index)
                 pos1'  (rem (+ pos1 move) 10)
                 args   [pos2 pos1' score2 (+ score1 (inc pos1'))]
                 cached (.get cache args)
                 [^long w2 ^long w1 :as r] (or cached (play args cache))]
             (when-not cached (.put cache args r))
             (recur (+ wins1 (* n w1))
               (+ wins2 (* n w2))
               (inc roll-index))))))))

(defn part-2
  [input]
  (apply max (play (parse-input input))))


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
