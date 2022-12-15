(ns aoc-2022.day-9
  (:require [aoc-utils :as u]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 9: Rope Bridge ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-9.txt"))

(def test-input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")
(def test-input-2 "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20")


(defn parse-input
  [input]
  (->> (u/read-as-vector input)
       (u/vpartition 2)
       (mapv (fn [[d n]]
               [(case (keyword d)
                  :L #(vector (dec ^long %1) %2)
                  :R #(vector (inc ^long %1) %2)
                  :U #(vector %1 (dec ^long %2))
                  :D #(vector %1 (inc ^long %2)))
                n]))))


;; This shall remain as a reminder of my inability to read instructions.
;; The visual example that was not in the description which breaks this approach
;; comes up because a piece of the tail can move diagonally:
;; _____    _____    _____
;; _____    _____    _____
;; _____ -> ___H_ -> __TH_
;; _TH__    _T___    _____
;; _____    _____    _____

(comment
  (defn adjust-bad
    [prev-parent
     [px py :as _new-parent]
     [kx ky :as knot]]
    (if (and (<= -1 (- kx px) 1)
             (<= -1 (- ky py) 1))
      knot
      prev-parent))
  ;
  )


(defn adjust
  [[^long px ^long py :as _parent]
   [^long x ^long y :as knot]]
  ;; still touching?
  (if (and (<= -1 (- x px) 1)
           (<= -1 (- y py) 1))
    knot
    [(+ x (Long/signum (- px x)))
     (+ y (Long/signum (- py y)))]))


(defn steps
  [[visited head tail] [move n]]
  (loop [visited visited
         [hx hy :as head] head
         [tail-head & tail-rest :as tail] tail
         i       0]
    (if (= i n)
      [visited head tail]
      (let [new-head (move hx hy)
            new-tail (reduce
                       (fn [new-tail knot]
                         (conj new-tail (adjust (peek new-tail) knot)))
                       [(adjust new-head tail-head)]
                       tail-rest)]
        (recur
          (conj! visited (peek new-tail))
          new-head
          new-tail
          (inc i))))))


(defn simulate-rope
  [input n]
  (->> (parse-input input)
       (reduce
         steps
         [(transient #{[0 0]})
          [0 0] (vec (repeat n [0 0]))])
       (first)
       count))


(defn part-1
  [input]
  (simulate-rope input 1))


(defn part-2
  [input]
  (simulate-rope input 9))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 13
  (part-1 task-input)                                       ; => 6470
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 1
  (part-2 test-input-2)                                     ; => 36
  (part-2 task-input)                                       ; => 2658
  (crit/quick-bench (part-2 task-input))

  )