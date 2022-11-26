(ns aoc-2016.day-1
  (:require [aoc-utils :as u]
            [clojure.set :as set]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 1: No Time for a Taxicab ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-1.txt"))

(def test-input "R8, R4, R4, R8")

(def north [0 1])
(def south [0 -1])
(def east [1 0])
(def west [-1 0])

(def turn-right
  (->> (cycle [north east south west])
       (partition 2 1)
       (take 4)
       (map vec)
       (into {})))

(def turn-left
  (set/map-invert turn-right))


(defn parse-input
  [input]
  (->> (u/read-as-vector input)
       (map name)
       (mapv (fn [op]
               [(if (= (first op) \R)
                  turn-right
                  turn-left)
                (parse-long (apply str (rest op)))]))))



(defn distance-to-finish
  [input walk-fn]
  (->> (parse-input input)
       (reduce
         walk-fn
         [[0 0] north #{}])
       ;; extract coordinates
       (first)
       (map abs)
       (apply +)))


(defn part-1
  [input]
  (distance-to-finish input
    (fn [[[x y] heading] [turn steps]]
      (let [[dx dy :as heading'] (turn heading)]
        [[(+ x (* dx steps))
          (+ y (* dy steps))]
         heading']))))


(defn part-2
  [input]
  (distance-to-finish input
    (fn [[position heading seen] [turn steps]]
      (let [[dx dy :as heading'] (turn heading)]
        (loop [[x y :as position] position
               seen seen
               n    steps]
          (cond
            (seen position) (reduced [position heading' seen])
            (zero? n) [position heading' seen]
            :else (recur
                    [(+ x dx) (+ y dy)]
                    (conj seen position)
                    (dec n))))))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 8
  (part-1 task-input)                                       ; => 279
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 4
  (part-2 task-input)                                       ; => 163
  (crit/quick-bench (part-2 task-input))

  )
