(ns aoc-2020.day-8
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 8:  --- https://adventofcode.com/2020/day/8

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-8.txt"))

;; instructions
(defn nop
  [state _]
  state)

(defn acc
  [state n]
  (update state :acc + n))

(defn jmp
  [state n]
  ;; decrement n to counteract the automatic :ip increment afterwards
  (update state :ip + (dec n)))


(defn dispatch
  [{:keys [ip ins] :as state}]
  (let [[op n] (ins ip)]
    (-> (op state n)
        (update :ip inc)
        (update :seen conj ip))))


(def base-state
  {:acc 0
   :ip  0
   :ins (->> task-input
             string/split-lines
             (map #(str "[" % "]"))
             (mapv read-string)
             eval)
   :seen #{}})


(def infinite-loop?
  [{:keys [seen ip] :as state}]
  (seen ip))


(comment
  ;; Part 1
  (->> (iterate dispatch base-state)
       (filter infinite-loop?)
       first
       :acc)
  ;; Part 2

  )
