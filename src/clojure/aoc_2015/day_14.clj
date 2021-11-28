(ns aoc-2015.day-14
  (:require [clojure.string :as string]
            [clojure.java.math :as math]
            [aoc-utils :as u]))

;; --- Day 14: Reindeer Olympics --- https://adventofcode.com/2015/day/14

(def task-input (u/slurp-resource "inputs/aoc_2015/day-14.txt"))

(def test-input "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

(def pattern #"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")


(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map #(re-find pattern %))
       (mapv (fn [[_ name speed dex rest]]
               {:name name
                :speed (parse-long speed)
                :dex (parse-long dex)
                :rest (parse-long rest)}))))


;; part 1 functions

(defn distance-steps
  [{:keys [speed dex rest]} target-time]
  (->> (concat (repeat dex speed) (repeat rest 0))
       (cycle)
       (take target-time)))


(defn distance-at
  [deer target-time]
  (apply + (distance-steps deer target-time)))


(defn part-1
  [input target-time]
  (->> (parse-input input)
       (map (juxt :name #(distance-at % target-time)))
       (sort-by second >)
       (first)))


;; part 2 functions

(defn accumulated-distances
  [deer target-time]
  (vec (reductions + (distance-steps deer target-time))))


(defn update-scores
  [distances scores step]
  (->> distances
       (sort-by #(nth (val %) step) >)
       (partition-by #(nth (val %) step))                   ;; catch multiple leaders in the same distance
       (first)
       (map key)
       (reduce
         #(update %1 %2 inc)
         scores)))


(defn part-2
  [input target-time]
  (let [deers (parse-input input)
        distances (into {} (map (juxt :name #(accumulated-distances % target-time)) deers))
        final-scores (reduce
                       #(update-scores distances %1 %2)
                       (into {} (map #(vector (:name %) 0) deers))
                       (range target-time))]
    (->> final-scores
         (sort-by val >)
         first)))


(comment
  ;; Part 1
  (part-1 test-input 1000)                                  ; => ["Comet" 1120]
  (part-1 task-input 2503)                                  ; => ["Vixen" 2660]

  ;; Part 2
  (part-2 test-input 1000)                                   ; => ["Dancer" 689]
  (part-2 task-input 2503)                                   ; => ["Blitzen" 1256]

  )
