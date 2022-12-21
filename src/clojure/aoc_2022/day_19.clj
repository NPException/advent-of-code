(ns aoc-2022.day-19
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 19: Not Enough Minerals ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-19.txt"))

(def test-input "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

; blueprint [id
;            ore-robot-ore-cost
;            clay-robot-ore-cost
;            obsidian-robot-ore-cost
;            obsidian-robot-clay-cost
;            geode-robot-ore-cost
;            geode-robot-obsidian-cost]
(defn parse-blueprint
  [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv parse-blueprint)))


(defn neighbour-states
  [{:keys [time-left bots materials]}
   [_id ore-bot-cost clay-bot-cost
    obsidian-bot-ore-cost obsidian-bot-clay-cost
    geode-bot-ore-cost geode-bot-obsidian-cost]]
  (let [time-left' (dec time-left)
        [b-ore b-clay b-obsidian b-geode] bots
        [ore clay obsidian _geode] materials
        [ore' clay' obsidian' geode' :as materials'] (mapv + bots materials)]
    (if (zero? time-left')
      [{:time-left time-left'
        :bots      bots
        :materials materials'}]
      (cond-> [{:time-left time-left'
                :bots      bots
                :materials materials'}]
        ; build ore-bot
        (>= ore ore-bot-cost)
        (conj {:time-left time-left'
               :bots      [(inc b-ore) b-clay b-obsidian b-geode]
               :materials [(- ore' ore-bot-cost) clay' obsidian' geode']})
        ; build clay-bot
        (>= ore clay-bot-cost)
        (conj {:time-left time-left'
               :bots      [b-ore (inc b-clay) b-obsidian b-geode]
               :materials [(- ore' clay-bot-cost) clay' obsidian' geode']})
        ; build obsidian-bot
        (and (>= ore obsidian-bot-ore-cost)
             (>= clay obsidian-bot-clay-cost))
        (conj {:time-left time-left'
               :bots      [b-ore b-clay (inc b-obsidian) b-geode]
               :materials [(- ore' obsidian-bot-ore-cost) (- clay' obsidian-bot-clay-cost) obsidian' geode']})
        ; build geode-bot
        (and (>= ore geode-bot-ore-cost)
             (>= obsidian geode-bot-obsidian-cost))
        (conj {:time-left time-left'
               :bots      [b-ore b-clay b-obsidian (inc b-geode)]
               :materials [(- ore' geode-bot-ore-cost) clay' (- obsidian' geode-bot-obsidian-cost) geode']})))))


(defn calculate-quality-level
  [blueprint]
  (take-last 2
    (u/A*-search
      [{:time-left 24
        ; [ore clay obsidian geode]
        :bots      [1 0 0 0]
        :materials [0 0 0 0]}]
      #(-> % :time-left zero?)
      #(neighbour-states % blueprint)
      (constantly 0)
      (fn [a b]
        ; TODO find a working cost function
        ))))


(defn part-1
  [input]
  )


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 33
  (part-1 task-input)                                       ; =>
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
