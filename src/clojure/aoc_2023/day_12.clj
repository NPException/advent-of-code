(ns aoc-2023.day-12
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit])
  (:import (java.util HashMap)))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 12: Hot Springs ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-12.txt"))

(def test-input "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1")

(defn parse-line
  [line]
  (u/split-parse line str #" " u/read-as-vector))


; my brain couldn't figure anything out that wasn't brute force, so I ended up translating this Java solution: https://github.com/dirk527/aoc2021/blob/main/src/aoc2023/Day12.java

; TODO: figure something out on your own. or at least try to understand how it works.

(defn new-state
  [^String ground numbers]
  {:ground        (str ground \.)                           ; sentinel value
   :numbers       (vec numbers)
   :current-group 0
   :ground-idx    0
   :number-idx    0
   :cache         (HashMap.)})


(declare count-possibilities)


(defn step-empty
  [{:keys [ground numbers current-group ground-idx number-idx cache] :as state}]
  (if (> current-group 0)
    (if (= (nth numbers number-idx) current-group)
      (count-possibilities
        (-> state
            (assoc :current-group 0)
            (assoc :ground-idx (inc ground-idx))
            (assoc :number-idx (inc number-idx))))
      0)
    (count-possibilities
      (-> state
          (assoc :current-group 0)
          (update :ground-idx inc)))))


(defn step-spring
  [{:keys [ground numbers current-group ground-idx number-idx cache] :as state}]
  (if (or (= number-idx (count numbers))
          (>= current-group (nth numbers number-idx)))
    ; cutoff: trying to parse as a spring, but no numbers left or group is already full
    0
    (count-possibilities
      (-> state
          (update :current-group inc)
          (update :ground-idx inc)))))


(defn count-possibilities
  [{:keys [ground numbers current-group ground-idx number-idx ^HashMap cache] :as state}]
  (let [cache-key [current-group ground-idx number-idx]]
    (if-let [cached-ret (.get cache cache-key)]
      cached-ret
      (let [ret (if (= ground-idx (count ground))
                  ; no more ground left, so no further recursion
                  (if (and (= current-group 0)
                           (= number-idx (count numbers)))
                    1                                       ; valid end state: all numbers were used up
                    0)
                  (case (nth ground ground-idx)
                    \. (step-empty state)
                    \# (step-spring state)
                    \? (+ (step-spring state)
                          (step-empty state))))]
        (.put cache cache-key ret)
        ret))))


(defn part-1
  [input]
  (->> (str/split-lines input)
       (map parse-line)
       (map #(apply new-state %))
       (map count-possibilities)
       (apply +)))


(defn part-2
  [input]
  (->> (str/split-lines input)
       (map parse-line)
       (map (fn [[row numbers]]
              (new-state
                (str/join \? (repeat 5 row))
                (apply concat (repeat 5 numbers)))))
       (map count-possibilities)
       (apply +)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 21
  (part-1 task-input)                                       ; => 7541
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 525152
  (part-2 task-input)                                       ; => 17485169859432
  (crit/quick-bench (part-2 task-input))

  )
