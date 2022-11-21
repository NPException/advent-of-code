(ns aoc-2015.day-21
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

;; --- Day 21: RPG Simulator 20XX ---

(def task-input (u/slurp-resource "inputs/aoc_2015/day-21.txt"))

(def test-input "Hit Points: 12\nDamage: 7\nArmor: 2")


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #": "))
       (map (fn [[k v]]
              [(-> (str/replace k " " "")
                   (str/lower-case)
                   keyword)
               (parse-long v)]))
       (into {})))


(def shop
  ;; [cost damage armor]
  {:weapons
   {:Dagger     [8 4 0]
    :Shortsword [10 5 0]
    :Warhammer  [25 6 0]
    :Longsword  [40 7 0]
    :Greataxe   [74 8 0]}
   :armor
   {:Leather    [13 0 1]
    :Chainmail  [31 0 2]
    :Splintmail [53 0 3]
    :Bandedmail [75 0 4]
    :Platemail  [102 0 5]}
   :rings
   {:Damage+1  [25 1 0]
    :Damage+2  [50 2 0]
    :Damage+3  [100 3 0]
    :Defense+1 [20 0 1]
    :Defense+2 [40 0 2]
    :Defense+3 [80 0 3]}})


(defn calc-damage
  ^long [^long damage-score ^long armor-score]
  (max 1 (- damage-score armor-score)))


(defn play
  "Returns true if the player beats the boss"
  [[^long player-hp player-damage player-armor]
   [^long boss-hp boss-damage boss-armor]]
  (let [player-attack (calc-damage player-damage boss-armor)
        boss-attack (calc-damage boss-damage player-armor)
        player-turns (long (math/ceil (/ (double boss-hp) player-attack)))
        boss-turns (long (math/ceil (/ (double player-hp) boss-attack)))]
    (<= player-turns boss-turns)))


(defn part-1
  [input]
  )


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; =>
  (part-1 task-input)                                       ; =>
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
