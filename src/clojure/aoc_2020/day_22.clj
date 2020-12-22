(ns aoc-2020.day-22
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 22: Crab Combat --- https://adventofcode.com/2020/day/22

(def task-input (u/slurp-resource "inputs/aoc_2020/day-22.txt"))

(def test-input "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10")


(defn parse-input
  [input]
  (->> (string/split input #"\n\n")
       (map #(read-string (str "[" (second (string/split % #":\n")) "]")))))


(defn play-game
  [round-fn players]
  (->> players
       (iterate round-fn)
       (drop-while (fn [[deck-1 deck-2]]
                     (and (seq deck-1) (seq deck-2))))
       first))


(defn determine-winner
  [input round-fn]
  (->> (parse-input input)
       (play-game round-fn)
       (filter seq)
       first
       reverse
       (map-indexed #(* (inc %1) %2))
       (apply +)))


;; part 1

(defn play-round
  [[[card-1 & rest-1] [card-2 & rest-2]]]
  (if (> card-1 card-2)
    [(into (vec rest-1) [card-1 card-2])
     rest-2]
    [rest-1
     (into (vec rest-2) [card-2 card-1])]))


(defn part-1
  [input]
  (determine-winner input play-round))


;; part 2

(defn play-recursive-round
  [[[card-1 & rest-1 :as deck-1]
    [card-2 & rest-2 :as deck-2]
    seen-1
    seen-2]]
  (let [new-seen-1 (conj (or seen-1 #{}) deck-1)
        new-seen-2 (conj (or seen-2 #{}) deck-2)]
    (cond
      ;; Instant player 1 win condition
      (or (and seen-1 (seen-1 deck-1))
          (and seen-2 (seen-2 deck-2)))
      [deck-1 nil]
      ;; Recurse into new game
      (and (>= (count rest-1) card-1)
           (>= (count rest-2) card-2))
      (let [[p1 _p2] (play-game
                       play-recursive-round
                       [(take card-1 rest-1) (take card-2 rest-2)])]
        (if (seq p1)
          [(into (vec rest-1) [card-1 card-2])
           (vec rest-2)
           new-seen-1
           new-seen-2]
          [(vec rest-1)
           (into (vec rest-2) [card-2 card-1])
           new-seen-1
           new-seen-2]))
      ;; Regular check
      :else
      (if (> card-1 card-2)
        [(into (vec rest-1) [card-1 card-2])
         rest-2
         new-seen-1
         new-seen-2]
        [rest-1
         (into (vec rest-2) [card-2 card-1])
         new-seen-1
         new-seen-2]))))


(defn part-2
  [input]
  (determine-winner input play-recursive-round))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 306
  (part-1 task-input)                                       ; => 32629

  ;; Part 2
  (part-2 test-input)                                       ; => 291
  (part-2 task-input)                                       ; => 32519

  )
