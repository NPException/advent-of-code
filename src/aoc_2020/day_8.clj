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


(def ops
  {'nop nop
   'acc acc
   'jmp jmp})


(defn dispatch
  [{:keys [ip ins] :as state}]
  (when-let [[op arg] (get ins ip)]
    (-> ((ops op) state arg)
        (update :ip inc)
        (update :seen conj ip))))


(def base-state
  {:acc  0
   :ip   0
   :ins  (->> task-input
              string/split-lines
              (map #(str "[" % "]"))
              (mapv read-string))
   :seen #{}})


(defn infinite-loop?
  [{:keys [seen ip] :as state}]
  (seen ip))


(defn find-last-state
  [state-seq]
  (loop [prev nil
         [state & remaining] state-seq]
    (cond
      (nil? state) prev
      (infinite-loop? state) state
      :else (recur state remaining))))


(defn run-to-end
  [state]
  (->> state
       (iterate dispatch)
       find-last-state))


;; for part 2
(defn bruteforce-flip-results
  [start-state]
  (for [i (range (-> start-state :ins count))
        :let [[op arg] (-> start-state :ins (nth i))
              flip-op ({'nop 'jmp
                        'jmp 'nop} op)]
        :when flip-op]
    (run-to-end (assoc-in start-state [:ins i] [flip-op arg]))))


(comment
  ;; Part 1 => 1563
  (->> base-state
       run-to-end
       :acc)
  ;; Part 2 => 767
  (->> base-state
       bruteforce-flip-results
       (remove infinite-loop?)
       first
       :acc)
  )
