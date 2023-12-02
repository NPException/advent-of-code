(ns aoc-2023.day-2
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 2: Cube Conundrum ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-2.txt"))

(def test-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn parse-draw
  [s]
  (->> (str/split (str/trim s) #", ")
       (into {:red 0, :green 0, :blue 0}
         (map (fn [die]
                (let [[n color] (str/split die #" ")]
                  [(keyword color) (parse-long n)]))))))

(defn parse-game
  [i s]
  (let [[_ draws] (str/split s #":")]
    [(inc i)
     (->> (str/split draws #";")
          (mapv parse-draw))]))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map-indexed parse-game)))


;; PART 1

; only 12 red cubes, 13 green cubes, and 14 blue cubes
(def max-red 12)
(def max-green 13)
(def max-blue 14)

(defn possible-draw?
  [{:keys [red green blue]}]
  (and (<= red max-red)
       (<= green max-green)
       (<= blue max-blue)))

(defn possible-game
  "Return id of the game if it was possible"
  [[id draws]]
  (when (every? possible-draw? draws)
    id))


; Determine which games would have been possible if the bag had been loaded
; with only 12 red cubes, 13 green cubes, and 14 blue cubes.
; What is the sum of the IDs of those games?
(defn part-1
  [input]
  (->> (parse-input input)
       (keep possible-game)
       (apply +)))


;; PART 2

(defn game-power
  [[_ draws]]
  (->> (reduce
         (fn [acc {:keys [red green blue]}]
           (-> (update acc :red max red)
               (update :green max green)
               (update :blue max blue)))
         draws)
       vals
       (apply *)))

; For each game, find the minimum set of cubes that must have been present.
; What is the sum of the power of these sets?
(defn part-2
  [input]
  (->> (parse-input input)
       (mapv game-power)
       (apply +)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 8
  (part-1 task-input)                                       ; => 2727
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 2286
  (part-2 task-input)                                       ; => 56580
  (crit/quick-bench (part-2 task-input))

  )
