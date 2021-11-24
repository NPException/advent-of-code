(ns aoc-2020.day-13
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 13: Shuttle Search --- https://adventofcode.com/2020/day/13

(def task-input (u/slurp-resource "inputs/aoc_2020/day-13.txt"))

(def test-input "939\n7,13,x,x,59,x,31,19")


(defn parse-input
  [^String input]
  (let [[time & bus-ids] (string/split input #"[\n,]")]
    [(parse-long time)
     (mapv parse-long bus-ids)]))


;; part 1

(defn minutes-to-wait
  [time bus-id]
  (- bus-id (mod time bus-id)))


(defn part-1
  [input]
  (let [[time bus-ids] (parse-input input)]
    (->> (remove nil? bus-ids)
         (sort-by #(minutes-to-wait time %))
         first
         (#(* % (minutes-to-wait time %))))))


;; part 2

;; note: this algorithm relies on the fact that all input ids are prime
(defn advance-matching-time
  [[time step] [index id]]
  [(some #(and (= 0 (mod (+ % index) id)) %)
         (iterate #(+ % step) time))
   (* step id)])

(defn part-2
  [input]
  (->> (parse-input input)
       second
       (map-indexed vector)
       (remove (comp nil? second))
       (reduce advance-matching-time [0 1])
       first))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 295
  (part-1 task-input)                                       ; => 2092

  ;; Part 2
  (part-2 test-input)                                       ; => 1068781
  (part-2 task-input)                                       ; => 702970661767766

  (map
    #(part-2 (str "\n" %))
    ["17,x,13,19"                                           ; 3417
     "67,7,59,61"                                           ; 754018
     "67,x,7,59,61"                                         ; 779210
     "67,7,x,59,61"                                         ; 1261476
     "1789,37,47,1889"])                                    ; 1202161486
  )
