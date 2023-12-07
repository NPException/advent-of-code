(ns aoc-2023.day-7
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 7: Camel Cards ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-7.txt"))

(def test-input "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv #(u/split-parse % str #" " parse-long))))


(def card-strength
  (->> (reverse '[A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2])
       (map-indexed #(vector (first (str %2)) %1))
       ; just to keep order for nicer debug readability
       (flatten)
       (apply array-map)))

(def card-strength-jokers
  (->> (reverse '[A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J])
       (map-indexed #(vector (first (str %2)) %1))
       ; just to keep order for nicer debug readability
       (flatten)
       (apply array-map)))


(defn hand-strength
  [jokers? hand]
  (let [freqs           (frequencies hand)
        jokers          (if jokers? (freqs \J 0) 0)
        freqs-no-jokers (if jokers? (dissoc freqs \J) freqs)
        labels          (count freqs-no-jokers)
        counts          (conj (set (vals freqs-no-jokers)) 0) ;; compensate for the case where jokers? is true, and the hand is "JJJJJ"
        most            (apply max counts)]
    (cond
      ; Five of a kind, where all five cards have the same label: AAAAA
      ; If jokers are allowed there are at max 2 labels, and one of them is J
      (= 5 (+ jokers most))
      7
      ; Four of a kind, where four cards have the same label and one card has a different label: AA8AA
      (= 4 (+ jokers most))
      6
      ; Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
      (and (= labels 2)
           (= 3 (+ jokers most)))
      5
      ; Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
      (= 3 (+ jokers most))
      4
      ; Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
      (and (= labels 3)
           (= jokers 0))
      3
      ; One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
      (= 2 (+ jokers most))
      2
      ; High card, where all cards' labels are distinct: 23456
      :else
      1)))


(defn solve-tie
  [jokers? hand-1 hand-2]
  (let [strength-fn (if jokers?
                      card-strength-jokers
                      card-strength)]
    (->> (mapv #(- (strength-fn %1) (strength-fn %2))
           hand-1 hand-2)
         (u/first-match #(not (zero? %))))))

(defn compare-hands
  [jokers? hand-1 hand-2]
  (if (= hand-1 hand-2)
    0
    (let [strength-1 (hand-strength jokers? hand-1)
          strength-2 (hand-strength jokers? hand-2)]
      (cond
        (< strength-1 strength-2) -1
        (> strength-1 strength-2) 1
        :else (solve-tie jokers? hand-1 hand-2)))))


(defn play-camel
  [with-jokers? input]
  (->> (parse-input input)
       (sort-by first (partial compare-hands with-jokers?))
       (map-indexed (fn [i [_ bid]]
                      (* (inc i) bid)))
       (apply +)))


(defn part-1
  [input]
  (play-camel false input))


(defn part-2
  [input]
  (play-camel true input))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 6440
  (part-1 task-input)                                       ; => 252656917
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 5905
  (part-2 task-input)                                       ; => 253499763
  (crit/quick-bench (part-2 task-input))

  )
