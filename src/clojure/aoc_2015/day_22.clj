(ns aoc-2015.day-22
  (:require [aoc-utils :as u]
            [aoc-2015.day-21 :as previous-day]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

;; --- Day 22: Wizard Simulator 20XX ---

(def task-input (u/slurp-resource "inputs/aoc_2015/day-22.txt"))

(let [[boss-hp boss-base-damage] (previous-day/parse-boss task-input)]
  (def boss-hp boss-hp)
  (def boss-base-damage boss-base-damage))

;; state:
;;   [used-mana player boss active-effects]
;; player:
;;   [hp mana]
;; boss:
;;   [hp damage]
;; active-effects: (always 3 elements. each element is the number of turns that the effect remains active)
;;   [shield poison recharge]

(def ^:const max-hp 50)

(def ^:const mana-index 0)
(def ^:const player-index 1)
(def ^:const boss-index 2)
(def ^:const effects-index 3)

(defn active?
  [^long effect-timer]
  (> effect-timer 0))

(defn win?
  [[_used-mana
    _player
    [boss-hp]]]
  (and boss-hp (<= boss-hp 0)))


(defn apply-effects!
  [[_used-mana
    [player-hp mana]
    [boss-hp _damage]
    [shield poison recharge]
    :as state§]]
  (let [boss-hp  (if (active? poison) (- boss-hp 3) boss-hp)
        armor    (if (active? shield) 7 0)
        new-mana (if (active? recharge) (+ mana 101) mana)]
    (-> state§
        (assoc! boss-index [boss-hp (- boss-base-damage armor)]) ;; update actual damage of boss this turn
        (assoc! player-index [player-hp new-mana])
        (assoc! effects-index [(dec shield) (dec poison) (dec recharge)]))))


(defn boss-attack!
  "Returns nil if the player dies from the attack"
  [[_used-mana
    [player-hp mana]
    [_boss-hp damage]
    _effects
    :as state§]]
  (when (< damage player-hp)
    (assoc! state§ player-index [(- player-hp damage) mana])))


(defn use-mana!
  [[used-mana
    [player-hp mana]
    :as state§]
   amount]
  (when (>= mana amount)
    (-> state§
        (assoc! mana-index (+ used-mana amount))
        (assoc! player-index [player-hp (- mana amount)]))))



;; Spell actions

;Magic Missile costs 53 mana. It instantly does 4 damage.
(defn cast-magic-missile!
  [[_used-mana
    _player
    [boss-hp damage]
    :as state§]]
  (some-> (use-mana! state§ 53)
          (assoc! boss-index [(- boss-hp 4) damage])))      ;; decrease boss health

;Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
(defn cast-drain!
  [[_used-mana
    [player-hp _mana]
    [boss-hp damage]
    :as state§]]
  (let [new-hp (min max-hp (+ player-hp 2))]
    (some-> (use-mana! state§ 73)
            (u/update! player-index assoc 0 new-hp)         ;; increase player health
            (assoc! boss-index [(- boss-hp 2) damage]))))   ;; decrease boss health

;Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
(defn cast-shield!
  [[_used-mana
    _player
    _boss
    [shield poison recharge]
    :as state§]]
  (when (not (active? shield))
    (some-> (use-mana! state§ 113)
            (assoc! effects-index [6 poison recharge]))))

;Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
(defn cast-poison!
  [[_used-mana
    _player
    _boss
    [shield poison recharge]
    :as state§]]
  (when (not (active? poison))
    (some-> (use-mana! state§ 173)
            (assoc! effects-index [shield 6 recharge]))))

;Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.
(defn cast-recharge!
  [[_used-mana
    _player
    _boss
    [shield poison recharge]
    :as state§]]
  (when (not (active? recharge))
    (some-> (use-mana! state§ 229)
            (assoc! effects-index [shield poison 5]))))


(defn full-turn
  [state player-action!]
  ;; start of player turn
  (let [state§ (apply-effects! (transient state))]
    (if (win? state§)
      (persistent! state§)
      ;; player action (using too much mana might lose the game)
      (when-let [state§ (player-action! state§)]
        (if (win? state§)
          (persistent! state§)
          ;; start of boss turn
          (let [state§ (apply-effects! state§)]
            (if (win? state§)
              (persistent! state§)
              ;; boss action (might kill the player)
              (some-> (boss-attack! state§)
                      (persistent!)))))))))


;; state:
;;   [used-mana player boss active-effects]
;; player:
;;   [hp mana]
;; boss:
;;   [hp damage]
;; active-effects: (always 3 elements. each element is the number of turns that the effect remains active)
;;   [shield poison recharge]

(defn part-1
  []
  (let [start-state [0
                     [50 500]
                     [boss-hp boss-base-damage]
                     [0 0 0]]]
    (-> (u/A*-search
          start-state
          win?
          (fn [state]
            (filterv some?
              [(full-turn state cast-magic-missile!)
               (full-turn state cast-drain!)
               (full-turn state cast-shield!)
               (full-turn state cast-poison!)
               (full-turn state cast-recharge!)]))
          (fn [_] 0)
          (fn [[mana] [next-mana]]
            (- next-mana mana)))
        ;; grab last path element
        (last)
        ;; used up mana
        (first))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1)                                                  ; => 900
  (crit/quick-bench (part-1))

  ;; Part 2
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
